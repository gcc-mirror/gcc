! { dg-do compile }
! { dg-options "-std=f2018" }
! PR fortran/99853

subroutine s1 ()
  select case (.true.) ! { dg-error "Cannot convert" }
  case (1_8)           ! { dg-error "must be of type LOGICAL" }
  end select
end

subroutine s2 ()
  select case (.false._1) ! { dg-error "Cannot convert" }
  case (2:3)              ! { dg-error "must be of type LOGICAL" }
  end select
end

subroutine s3 ()
  select case (3_2) ! { dg-error "Cannot convert" }
  case (.false.)    ! { dg-error "must be of type INTEGER" }
  end select
end

subroutine s4 (i)
  select case (i) ! { dg-error "Cannot convert" }
  case (.true._8) ! { dg-error "must be of type INTEGER" }
  end select
end

! { dg-prune-output "Cannot convert" }
