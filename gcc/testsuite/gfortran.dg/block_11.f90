! { dg-do link }
!
! PR fortran/52729
!
! Based on a contribution of Andrew Benson
!
module testMod
  type testType
  end type testType
contains
  subroutine testSub()
    implicit none
    procedure(double precision ), pointer :: r
    class    (testType         ), pointer :: testObject
    double precision                      :: testVal

    ! Failed as testFunc was BT_UNKNOWN
    select type (testObject)
    class is (testType)
       testVal=testFunc()
       r => testFunc
    end select
    return
  end subroutine testSub

  double precision function testFunc()
    implicit none
    return
  end function testFunc
end module testMod

module testMod2
  implicit none
contains
  subroutine testSub()
    procedure(double precision ), pointer :: r
    double precision                      :: testVal
    ! Failed as testFunc was BT_UNKNOWN
    block
      r => testFunc
      testVal=testFunc()
    end block
  end subroutine testSub

  double precision function testFunc()
  end function testFunc
end module testMod2

module m3
  implicit none
contains
  subroutine my_test()
    procedure(), pointer :: ptr
    ! Before the fix, one had the link error
    ! "undefined reference to `sub.1909'"
    block
      ptr => sub
      call sub()
    end block
  end subroutine my_test
  subroutine sub(a)
    integer, optional :: a
  end subroutine sub
end module m3

end
