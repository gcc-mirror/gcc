! { dg-do compile }
! { dg-options "-w" }

program c_by_val_2
  external bar
  real (4) :: bar, ar(2) = (/1.0,2.0/)
  type     :: mytype
    integer  :: i
  end type mytype
  type(mytype)  :: z
  character(8)  :: c = "blooey"
  print *, sin (%VAL(2.0))   ! { dg-error "not allowed in this context" }
  print *, foo (%VAL(1.0))   ! { dg-error "not allowed in this context" }
  call  foobar (%VAL(0.5))   ! { dg-error "not allowed in this context" }
  print *, bar (%VAL(z))     ! { dg-error "not of numeric type" }
  print *, bar (%VAL(c))     ! { dg-error "not of numeric type" }
  print *, bar (%VAL(ar))    ! { dg-error "cannot be an array" }
  print *, bar (%VAL(0.0))
contains
  function foo (a)
    real(4) :: a, foo
    foo = cos (a)
  end function foo
  subroutine foobar (a)
    real(4) :: a
    print *, a
  end subroutine foobar
end program c_by_val_2

