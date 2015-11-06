! { dg-do compile }

program tile
  integer i, j, a

  !$acc parallel default (shared) ! { dg-error "Unclassifiable OpenACC directive" }
  !$acc end parallel ! { dg-error "Unexpected" }

  !$acc parallel default (private) ! { dg-error "Unclassifiable OpenACC directive" }
  !$acc end parallel ! { dg-error "Unexpected" }

  !$acc parallel default (none)
  !$acc end parallel

  !$acc parallel default (firstprivate) ! { dg-error "Unclassifiable OpenACC directive" }
  !$acc end parallel ! { dg-error "Unexpected" }
end program tile
