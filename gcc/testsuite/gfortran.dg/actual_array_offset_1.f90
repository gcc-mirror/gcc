! { dg-do run }
!
! Check the fix for PR67779, in which array sections passed in the
! recursive calls to 'quicksort' had an incorrect offset.
!
! Contributed by Arjen Markus  <arjen.markus895@gmail.com>
!
! NOTE: This is the version of the testcase in comment #16 (from Thomas Koenig)
!
module myclass_def
    implicit none

    type, abstract :: myclass
    contains
        procedure(assign_object), deferred        :: copy
        procedure(one_lower_than_two), deferred   :: lower
        procedure(print_object), deferred         :: print
        procedure, nopass                         :: quicksort  ! without nopass, it does not work
    end type myclass

    abstract interface
        subroutine assign_object( left, right )
            import                        :: myclass
            class(myclass), intent(inout) :: left
            class(myclass), intent(in)    :: right
        end subroutine assign_object
    end interface

    abstract interface
        logical function one_lower_than_two( op1, op2 )
            import                     :: myclass
            class(myclass), intent(in) :: op1, op2
        end function one_lower_than_two
    end interface

    abstract interface
        subroutine print_object( obj )
            import                     :: myclass
            class(myclass), intent(in) :: obj
        end subroutine print_object
    end interface

    !
    ! Type containing a real
    !

    type, extends(myclass) :: mysortable
        integer :: value
    contains
        procedure :: copy    => copy_sortable
        procedure :: lower   => lower_sortable
        procedure :: print   => print_sortable
    end type mysortable

contains
!
! Generic part
!
recursive subroutine quicksort( array )
    class(myclass), dimension(:) :: array

    class(myclass), allocatable :: v, tmp
    integer                     :: i, j

    integer :: k

    i = 1
    j = size(array)

    allocate( v,   source = array(1) )
    allocate( tmp, source = array(1) )

    call v%copy( array((j+i)/2) ) ! Use the middle element

    do
        do while ( array(i)%lower(v) )
            i = i + 1
        enddo
        do while ( v%lower(array(j)) )
            j = j - 1
        enddo

        if ( i <= j ) then
            call tmp%copy( array(i) )
            call array(i)%copy( array(j) )
            call array(j)%copy( tmp )
            i        = i + 1
            j        = j - 1
        endif

        if ( i > j ) then
            exit
        endif
    enddo

    if ( 1 < j ) then
        call quicksort( array(1:j) ) ! Problem here
    endif

    if ( i < size(array) ) then
        call quicksort( array(i:) )  ! ....and here
    endif
end subroutine quicksort

!
! Specific part
!
subroutine copy_sortable( left, right )
    class(mysortable), intent(inout) :: left
    class(myclass), intent(in)       :: right

    select type (right)
        type is (mysortable)
            select type (left)
                type is (mysortable)
                    left = right
            end select
    end select
end subroutine copy_sortable

logical function lower_sortable( op1, op2 )
    class(mysortable), intent(in) :: op1
    class(myclass),    intent(in) :: op2

    select type (op2)
        type is (mysortable)
            lower_sortable = op1%value < op2%value
    end select
end function lower_sortable

subroutine print_sortable( obj )
    class(mysortable), intent(in) :: obj

    write(*,'(G0," ")', advance="no") obj%value
end subroutine print_sortable

end module myclass_def


! test program
program test_quicksort
    use myclass_def

    implicit none

    type(mysortable), dimension(20) :: array
    real, dimension(20) :: values

    call random_number(values)

    array%value = int (1000000 * values)

! It would be pretty perverse if this failed!
    if (check (array)) call abort

    call quicksort( array )

! Check the the array is correctly ordered
    if (.not.check (array)) call abort
contains
     logical function check (arg)
         type(mysortable), dimension(:) :: arg
         integer                        :: s
         s = size (arg, 1)
         check = all (arg(2 : s)%value .ge. arg(1 : s - 1)%value)
     end function check
end program test_quicksort
