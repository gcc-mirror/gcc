! { dg-do compile { target x86_64-*-* } }
! { dg-additional-options "-foffload=disable" }

program main
contains
  subroutine f01 ()
  end subroutine
  subroutine f02 ()
    !$omp declare variant (f01) &
    !$omp&  match (device={kind (score(5) : host)})
  ! { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-1 }
  end subroutine
  subroutine f03 ()
  end subroutine
  subroutine f04 ()
    !$omp declare variant (f03) &
    !$omp&  match (device={kind (host), arch (score(6) : x86_64), isa (avx512f)})
  ! { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-1 }
  end subroutine
  subroutine f05 ()
  end subroutine
  subroutine f06 ()
    !$omp declare variant (f05) &
    !$omp&  match (device={kind (host), arch (score(6) : x86_64), &
    !$omp&                 isa (score(7): avx512f)})
  ! { dg-error ".score. cannot be specified in traits in the .device. trait-selector-set" "" { target *-*-*} .-2 }
  end subroutine

end program

