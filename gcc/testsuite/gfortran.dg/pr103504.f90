! { dg-do compile }
! PR fortran/103504 - ICE in get_sym_storage_size, at fortran/interface.c:2800
! Contributed by G.Steinmetz

program p
  implicit none
  real      :: y(1)
  character :: b
  call s(y)
  call t(y)
  call u(y)
  call c(b)
contains
  subroutine s(x)
    real :: x(abs(1.):1)        ! { dg-error "must be of INTEGER type" }
  end
  subroutine t(x)
    real :: x(abs(1.):1)        ! { dg-error "must be of INTEGER type" }
  end
  subroutine u(x)
    real :: x(1:abs(1.))        ! { dg-error "must be of INTEGER type" }
  end
  subroutine c(z)
    character(len=abs(1.)) :: z ! { dg-error "must be of INTEGER type" }
  end subroutine c
end

! { dg-prune-output "must be of INTEGER type" }
