! { dg-do run }
! Test the fix for PR34438, in which default initializers
! forced the derived type to be static; ie. initialized once
! during the lifetime of the programme.  Instead, they should
! be initialized each time they come into scope.
!
! Contributed by Sven Buijssen <sven.buijssen@math.uni-dortmund.de>
! Third test is from  Dominique Dhumieres <dominiq@lps.ens.fr>
!
module demo
   type myint
     integer :: bar = 42
   end type myint
end module demo

! As the name implies, this was the original testcase
! provided by the contributor....
subroutine original
  use demo
  integer val1 (6)
  integer val2 (6)
  call recfunc (1)
  if (any (val1 .ne. (/1, 2, 3, 1, 2, 3/))) STOP 1
  if (any (val2 .ne. (/1, 2, 3, 4, 4, 4/))) STOP 2
contains

  recursive subroutine recfunc (ivalue)
    integer, intent(in) :: ivalue
    type(myint) :: foo1
    type(myint) :: foo2 = myint (99)
    foo1%bar = ivalue
    foo2%bar = ivalue
    if (ivalue .le. 3) then
      val1(ivalue) = foo1%bar
      val2(ivalue) = foo2%bar
      call recfunc (ivalue + 1)
      val1(ivalue + 3) = foo1%bar
      val2(ivalue + 3) = foo2%bar
    endif
  end subroutine recfunc
end subroutine original

! ...who came up with this one too.
subroutine func (ivalue, retval1, retval2)
  use demo
  integer, intent(in) :: ivalue
  type(myint) :: foo1
  type(myint) :: foo2 = myint (77)
  type(myint) :: retval1
  type(myint) :: retval2
  retval1 = foo1
  retval2 = foo2
  foo1%bar = 999
  foo2%bar = 999
end subroutine func

subroutine other
  use demo
  interface
    subroutine func(ivalue, rv1, rv2)
      use demo
      integer, intent(in) :: ivalue
      type(myint) :: foo, rv1, rv2
   end subroutine func
  end interface
  type(myint) :: val1, val2
  call func (1, val1, val2)
  if ((val1%bar .ne. 42) .or. (val2%bar .ne. 77)) STOP 3
  call func (2, val1, val2)
  if ((val1%bar .ne. 42) .or. (val2%bar .ne. 999)) STOP 4

end subroutine other

MODULE M1
  TYPE T1
    INTEGER :: i=7
  END TYPE T1
CONTAINS
  FUNCTION F1(d1) RESULT(res)
    INTEGER :: res
    TYPE(T1), INTENT(OUT) :: d1
    TYPE(T1), INTENT(INOUT) :: d2
    res=d1%i
    d1%i=0
    RETURN
  ENTRY   E1(d2) RESULT(res)
    res=d2%i
    d2%i=0
  END FUNCTION F1
END MODULE M1

! This tests the fix of a regression caused by the first version
! of the patch.
subroutine dominique ()
  USE M1
  TYPE(T1) :: D1
  D1=T1(3)
  if (F1(D1) .ne. 7) STOP 5
  D1=T1(3)
  if (E1(D1) .ne. 3) STOP 6
END

! Run both tests.
  call original
  call other
  call dominique
end
