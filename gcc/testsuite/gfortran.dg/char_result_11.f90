! { dg-do link }
! PR 23675: Character function of module-variable length
! PR 25716: Implicit kind conversions in in expressions written to *.mod-files.
module cutils

    implicit none
    private
   
    type t
        integer :: k = 25
        integer :: kk(3) = (/30, 40, 50 /)
    end type t

    integer :: m1 = 25, m2 = 25, m3 = 25, m4 = 25, m5 = 25
    integer :: n5 = 3, n7 = 3, n9 = 3
    integer(1) :: n1 = 3, n2 = 3, n3 = 3, n4 = 3, n6 = 3, n8 = 3
    character(10) :: s = "abcdefghij"
    integer :: x(4) = (/ 30, 40, 50, 60 /)
    type(t), save :: tt1(5), tt2(5)

    public :: IntToChar1, IntToChar2, IntToChar3, IntToChar4, IntToChar5, &
                IntToChar6, IntToChar7, IntToChar8

contains

    pure integer function get_k(tt)
        type(t), intent(in) :: tt

        get_k = tt%k
    end function get_k
 
    function IntToChar1(integerValue) result(a)
        integer, intent(in) :: integerValue
        character(len=m1)  :: a
 
        write(a, *) integerValue
    end function IntToChar1
 
    function IntToChar2(integerValue) result(a)
        integer, intent(in) :: integerValue
        character(len=m2+n1)  :: a
 
        write(a, *) integerValue
    end function IntToChar2
 
    function IntToChar3(integerValue) result(a)
        integer, intent(in) :: integerValue
        character(len=iachar(s(n2:n3)))  :: a
 
        write(a, *) integerValue
    end function IntToChar3
 
    function IntToChar4(integerValue) result(a)
        integer, intent(in) :: integerValue
        character(len=tt1(n4)%k)  :: a
 
        write(a, *) integerValue
    end function IntToChar4
 
    function IntToChar5(integerValue) result(a)
        integer, intent(in) :: integerValue
        character(len=maxval((/m3, n5/)))  :: a
 
        write(a, *) integerValue
    end function IntToChar5
 
    function IntToChar6(integerValue) result(a)
        integer, intent(in) :: integerValue
        character(len=x(n6))  :: a
 
        write(a, *) integerValue
    end function IntToChar6
 
    function IntToChar7(integerValue) result(a)
        integer, intent(in) :: integerValue
        character(len=tt2(min(m4, n7, 2))%kk(n8))  :: a
     
        write(a, *) integerValue
    end function IntToChar7
 
    function IntToChar8(integerValue) result(a)
        integer, intent(in) :: integerValue
        character(len=get_k(t(m5, (/31, n9, 53/))))  :: a
 
        write(a, *) integerValue
    end function IntToChar8

end module cutils


program test

    use cutils

    implicit none
    character(25) :: str
    
    str = IntToChar1(3)
    print *, str
    str = IntToChar2(3)
    print *, str
    str = IntToChar3(3)
    print *, str
    str = IntToChar4(3)
    print *, str
    str = IntToChar5(3)
    print *, str
    str = IntToChar6(3)
    print *, str
    str = IntToChar7(3)
    print *, str
    str = IntToChar8(3)
    print *, str

end program test

! { dg-final { cleanup-modules "cutils" } }
