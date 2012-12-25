! { dg-do compile }
! { dg-options "-O2" }
!
! PR fortran/54884
!
! Check that get_key_len is not optimized away as it
! is used in a publicly visible specification expression.
!

module m
  private
  public :: foo
  interface foo
    module procedure bar
  end interface foo
contains
  pure function mylen()
    integer :: mylen
    mylen = 42
  end function mylen
  pure function myotherlen()
    integer :: myotherlen
    myotherlen = 99
  end function myotherlen
  subroutine bar(x)
    character(len=mylen()) :: x
    character :: z2(myotherlen())
    call internal(x)
    block
       character(len=myotherlen()) :: z
       z = "abc"
       x(1:5) = z
    end block
    x(6:10) = intern_func()
  contains
    function intern_func()
      character(len=myotherlen()) :: intern_func
      intern_func = "zuzu"
    end function intern_func
   subroutine internal(y)
      character(len=myotherlen()) :: y
      y = "abc"
    end subroutine internal
  end subroutine bar
end module m

! { dg-final { scan-assembler-not "__m_MOD_myotherlen" } }
! { dg-final { scan-assembler "__m_MOD_bar" } }
! { dg-final { scan-assembler "__m_MOD_mylen" } }
