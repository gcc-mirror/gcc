! { dg-do compile }
! PR fortran/103776 - ICE in gfc_compare_string
! Contributed by G.Steinmetz

program p
  integer :: n
  select case (n)
  case ([1])        ! { dg-error "must be scalar" }
  end select
  select case (n)
  case (:[2])       ! { dg-error "must be scalar" }
  end select
  select case (n)
  case (['1'])      ! { dg-error "must be scalar" }
  end select
  select case (n)
  case (['1']:2)    ! { dg-error "must be scalar" }
  end select
  select case (n)
  case(['1']:['2']) ! { dg-error "must be scalar" }
  end select
  select case (n)
  case(1:['2'])     ! { dg-error "must be scalar" }
  end select
end
