! { dg-do run }
! PR fortran/107721 - array constructor type-spec lost with parentheses
! PR fortran/102417 - character array constructor type-spec lost
!
! Tests type-spec preservation in array constructors with parentheses,
! nested constructors, and CLASS(*) type verification for all intrinsic types.

program array_constructor_typespec_1
    implicit none
    integer :: i, iscalar
    integer, dimension(2) :: iarr
    real, dimension(2) :: rarr
    real :: rscalar
    complex, dimension(2) :: carr
    complex :: cscalar
    logical, dimension(2) :: larr
    character(4), dimension(3) :: charr
    character(8), dimension(2) :: charr8
    character(16), dimension(3) :: charr16
    character(16), dimension(2) :: charr16_2
    character(:), allocatable :: charr17(:)
    character :: x = 'a', y = 'b'
    class(*), allocatable :: res(:)
    character(10), dimension(1) :: charr10
    character(4), dimension(1) :: charr4_1
    character(:), allocatable :: charr0(:)
    character(4), dimension(0) :: empty4

    ! INTEGER - runtime value checks
    iarr = [integer :: [1.0], [2.0]]
    if (any(iarr /= [1, 2])) stop 1
    iarr = [integer :: ([1.0]), ([2.0])]
    if (any(iarr /= [1, 2])) stop 2
    iarr = [integer :: ((([1.0]))), [2.0]]
    if (any(iarr /= [1, 2])) stop 3

    ! REAL - runtime value checks
    rarr = [real :: [2], [3]]
    if (any(rarr /= [2.0, 3.0])) stop 4
    rarr = [real :: ([2]), ([3])]
    if (any(rarr /= [2.0, 3.0])) stop 5
    rarr = [real :: ((([2]))), [3]]
    if (any(rarr /= [2.0, 3.0])) stop 6

    ! COMPLEX - runtime value checks
    carr = [complex :: [3], [4]]
    if (any(carr /= [(3.0, 0.0), (4.0, 0.0)])) stop 7
    carr = [complex :: ([3]), ([4])]
    if (any(carr /= [(3.0, 0.0), (4.0, 0.0)])) stop 8
    carr = [complex :: ((([3]))), [4]]
    if (any(carr /= [(3.0, 0.0), (4.0, 0.0)])) stop 9

    ! LOGICAL - runtime value checks
    larr = [logical :: [.true.], [.false.]]
    if (any(larr .neqv. [.true., .false.])) stop 10
    larr = [logical :: ([.true.]), ([.false.])]
    if (any(larr .neqv. [.true., .false.])) stop 11

    ! CHARACTER - runtime value checks (PR 102417)
    charr = [character(4) :: 'a', 'b', 'c']
    if (any(charr /= ['a   ', 'b   ', 'c   '])) stop 12
    charr = [character(4) :: ('a'), 'b', 'c']
    if (any(charr /= ['a   ', 'b   ', 'c   '])) stop 13
    charr = [[character(4) :: 'a', 'b', 'c']]
    if (any(charr /= ['a   ', 'b   ', 'c   '])) stop 14

    ! CHARACTER with nested constructors - length 8
    charr8 = [character(8) :: 'x', 'y']
    if (charr8(1) /= 'x       ') stop 15
    if (charr8(2) /= 'y       ') stop 16

    charr8 = [character(8) :: ['a', 'b']]
    if (charr8(1) /= 'a       ') stop 17
    if (charr8(2) /= 'b       ') stop 18

    ! Outer constructor without type-spec, inner with type-spec.
    ! With proper type-spec propagation, no length mismatch warning is needed.
    charr8 = [[character(8) :: ['a', 'b']]]
    if (charr8(1) /= 'a       ') stop 19
    if (charr8(2) /= 'b       ') stop 20

    ! Triple-nested constructor with type-spec in middle.
    charr8 = [[[character(8) :: ['a', 'b']]]]
    if (charr8(1) /= 'a       ') stop 21
    if (charr8(2) /= 'b       ') stop 22

    charr8 = [character(8) :: (x), (y)]
    if (charr8(1) /= 'a       ') stop 23
    if (charr8(2) /= 'b       ') stop 24

    charr8 = [[character(8) :: (x), (y)]]
    if (charr8(1) /= 'a       ') stop 25
    if (charr8(2) /= 'b       ') stop 26

    ! CHARACTER concatenation with parentheses (PR 107721 comment 14)
    charr16_2 = [character(16) :: 'a' // 'c', 'b' // 'de']
    if (charr16_2(1) /= 'ac              ') stop 101
    if (charr16_2(2) /= 'bde             ') stop 102

    charr16_2 = [character(16) :: 'a' // 'c', ('b' // 'de')]
    if (charr16_2(1) /= 'ac              ') stop 103
    if (charr16_2(2) /= 'bde             ') stop 104

    charr16_2 = [character(16) :: ('a' // 'c'), 'b' // 'de']
    if (charr16_2(1) /= 'ac              ') stop 105
    if (charr16_2(2) /= 'bde             ') stop 106

    ! CHARACTER concatenation after constructor - verify length 17
    charr17 = [character(16) :: 'a' // 'c', 'b' // 'de'] // '|'
    if (len(charr17) /= 17) stop 107
    if (charr17(1) /= 'ac              |') stop 108
    if (charr17(2) /= 'bde             |') stop 109

    charr17 = [character(16) :: 'a' // 'c', ('b' // 'de')] // '|'
    if (len(charr17) /= 17) stop 110
    if (charr17(1) /= 'ac              |') stop 111
    if (charr17(2) /= 'bde             |') stop 112

    charr17 = [character(16) :: ('a' // 'c'), 'b' // 'de'] // '|'
    if (len(charr17) /= 17) stop 113
    if (charr17(1) /= 'ac              |') stop 114
    if (charr17(2) /= 'bde             |') stop 115

    ! CHARACTER - longer length 16
    charr16 = [character(16) :: 'a', 'b', 'c']
    if (charr16(1) /= 'a               ') stop 27
    if (charr16(2) /= 'b               ') stop 28
    if (charr16(3) /= 'c               ') stop 29

    charr16 = [[character(16) :: 'a', 'b', 'c']]
    if (charr16(1) /= 'a               ') stop 30
    if (charr16(2) /= 'b               ') stop 31
    if (charr16(3) /= 'c               ') stop 32

    ! CHARACTER - truncation cases
    charr8 = [character(8) :: 'abcdefghij', 'klmnopqrst']
    if (charr8(1) /= 'abcdefgh') stop 33
    if (charr8(2) /= 'klmnopqr') stop 34

    charr8 = [[character(8) :: 'abcdefghij', 'klmnopqrst']]
    if (charr8(1) /= 'abcdefgh') stop 35
    if (charr8(2) /= 'klmnopqr') stop 36

    ! Implied-do with parentheses
    iarr = [integer :: (/(1.0*i, i=1, 2)/)]
    if (any(iarr /= [1, 2])) stop 37
    iarr = [integer :: ((/(1.0*i, i=1, 2)/))]
    if (any(iarr /= [1, 2])) stop 38

    ! Type verification with CLASS(*) - ensure types are actually converted
    res = [integer :: ([1.0])]
    call verify_integer (res, 42)
    deallocate (res)

    res = [integer :: ([1.0]), ([2.0])]
    call verify_integer (res, 43)
    deallocate (res)

    res = [real :: ([2]), [3]]
    call verify_real (res, 44)
    deallocate (res)

    res = [complex :: ([3])]
    call verify_complex (res, 45)
    deallocate (res)

    res = [logical :: ([.true.]), [.false.]]
    call verify_logical (res, 46)
    deallocate (res)

    ! Parenthesized constructors - verify result TYPE not just value
    res = [integer :: ([1.0])] ** 2
    call verify_integer (res, 47)
    deallocate (res)

    res = [real :: ([2]), [3]] ** 2
    call verify_real (res, 48)
    deallocate (res)

    res = [complex :: ([3])] ** 2
    call verify_complex (res, 49)
    deallocate (res)

    ! Harald's test cases from Comment #17 - scalar // constructor patterns
    charr17 = '|' // [character(16) :: 'a' // 'c', 'b' // 'de']
    if (len(charr17) /= 17) stop 116
    if (charr17(1) /= '|ac              ') stop 117
    if (charr17(2) /= '|bde             ') stop 118

    charr17 = '|' // [character(16) :: 'a' // 'c', ('b' // 'de')]
    if (len(charr17) /= 17) stop 119
    if (charr17(1) /= '|ac              ') stop 120
    if (charr17(2) /= '|bde             ') stop 121

    charr17 = '|' // [character(16) :: ('a' // 'c'), 'b' // 'de']
    if (len(charr17) /= 17) stop 122
    if (charr17(1) /= '|ac              ') stop 123
    if (charr17(2) /= '|bde             ') stop 124

    ! Comment #11: Nested array constructor with concatenation
    ! The inner ['a','b'] must be padded to length 16 before concat
    charr17 = [character(16) :: ['a', 'b']] // '|'
    if (len(charr17) /= 17) stop 125
    if (charr17(1) /= 'a               |') stop 126
    if (charr17(2) /= 'b               |') stop 127

    ! Comment #18: Outer constructor without type-spec wrapping inner with
    ! type-spec.  The type-spec must be propagated when flattening.
    charr17 = [[character(16) :: ['a', 'b']]] // '|'
    if (len(charr17) /= 17) stop 128
    if (charr17(1) /= 'a               |') stop 129
    if (charr17(2) /= 'b               |') stop 130

    charr17 = '|' // [[character(16) :: ['a', 'b']]]
    if (len(charr17) /= 17) stop 131
    if (charr17(1) /= '|a               ') stop 132
    if (charr17(2) /= '|b               ') stop 133

    ! Harald's new test cases from Comment #22 - nested truncation and padding
    ! [ character(2) :: ['abcd','efgh'] ] should truncate to 'ab', 'ef'
    ! Then [ character(16) :: ... ] should pad to 'ab              ', 'ef              '

    charr16_2 = [ character(16) ::  [ character(2) :: ['abcd','efgh'] ] ]
    if (charr16_2(1) /= 'ab              ') stop 134
    if (charr16_2(2) /= 'ef              ') stop 135

    charr17 = [ character(16) ::  [ character(2) :: ['abcd','efgh'] ] ] // "|"
    if (len(charr17) /= 17) stop 136
    if (charr17(1) /= 'ab              |') stop 137
    if (charr17(2) /= 'ef              |') stop 138

    charr17 = "|" // [ character(16) ::  [ character(2) :: ['abcd','efgh'] ] ]
    if (len(charr17) /= 17) stop 139
    if (charr17(1) /= '|ab              ') stop 140
    if (charr17(2) /= '|ef              ') stop 141

    charr16_2 = [ character(16) :: ([ character(2) :: ['abcd','efgh'] ])]
    if (charr16_2(1) /= 'ab              ') stop 142
    if (charr16_2(2) /= 'ef              ') stop 143

    charr17 = [ character(16) :: ([ character(2) :: ['abcd','efgh'] ])] // "|"
    if (len(charr17) /= 17) stop 144
    if (charr17(1) /= 'ab              |') stop 145
    if (charr17(2) /= 'ef              |') stop 146

    charr17 = "|" // [ character(16) :: ([ character(2) :: ['abcd','efgh'] ])]
    if (len(charr17) /= 17) stop 147
    if (charr17(1) /= '|ab              ') stop 148
    if (charr17(2) /= '|ef              ') stop 149
    deallocate (charr17)

    ! Additional torture tests
    ! Triple nesting with explicit types: 'abcde'(5) -> 'ab'(2) -> 'ab        '(10)
    charr10 = [character(10) :: [character(2) :: [character(5) :: 'abcde']]]
    if (charr10(1) /= 'ab        ') stop 150

    ! Concatenation of constructors
    ! 'a'(2) // 'b'(3) -> 'a b  '(5) -> 'a b '(4)
    charr4_1 = [character(4) :: [character(2) :: 'a'] // [character(3) :: 'b']]
    if (charr4_1(1) /= 'a b ') stop 151

    ! Zero length strings
    ! Inner zero length: 'abc' -> ''(0) -> '    '(4)
    charr4_1 = [character(4) :: [character(0) :: 'abc']]
    if (charr4_1(1) /= '    ') stop 152

    ! Outer zero length: 'abc' -> 'abc '(4) -> ''(0)
    charr0 = [character(0) :: [character(4) :: 'abc']]
    if (len(charr0) /= 0) stop 153
    if (charr0(1) /= '') stop 154
    deallocate (charr0)

    ! Empty array constructors
    empty4 = [character(4) :: ]
    if (size(empty4) /= 0) stop 155
    
    empty4 = [character(4) :: [character(2) :: ]]
    if (size(empty4) /= 0) stop 156

contains

    subroutine verify_integer (x, stopcode)
        class(*), intent(in) :: x(:)
        integer,  intent(in) :: stopcode
        select type (x)
        type is (integer)
            ! Correct type
        class default
            stop stopcode
        end select
    end subroutine verify_integer

    subroutine verify_real (x, stopcode)
        class(*), intent(in) :: x(:)
        integer,  intent(in) :: stopcode
        select type (x)
        type is (real)
            ! Correct type
        class default
            stop stopcode
        end select
    end subroutine verify_real

    subroutine verify_complex (x, stopcode)
        class(*), intent(in) :: x(:)
        integer,  intent(in) :: stopcode
        select type (x)
        type is (complex)
            ! Correct type
        class default
            stop stopcode
        end select
    end subroutine verify_complex

    subroutine verify_logical (x, stopcode)
        class(*), intent(in) :: x(:)
        integer,  intent(in) :: stopcode
        select type (x)
        type is (logical)
            ! Correct type
        class default
            stop stopcode
        end select
    end subroutine verify_logical

end program array_constructor_typespec_1
