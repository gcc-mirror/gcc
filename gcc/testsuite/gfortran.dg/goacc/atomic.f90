! { dg-do compile }

subroutine foo
  integer :: i, v
  !$acc atomic read bar  ! { dg-error "21: Unexpected junk after !.ACC ATOMIC statement" }
  i = v

  !$acc atomic read write  ! { dg-error "21: Unexpected junk after !.ACC ATOMIC statement" }
  i = v

  !$acc atomic read seq_cst  ! { dg-error "21: Unexpected junk after !.ACC ATOMIC statement" }
  i = v

  !$acc atomic read relaxed  ! { dg-error "21: Unexpected junk after !.ACC ATOMIC statement" }
  i = v

  !$acc atomic update hint(1)  ! { dg-error "23: Unexpected junk after !.ACC ATOMIC statement" }
  i = i + 1

  !$acc atomic update update capture  ! { dg-error "23: Unexpected junk after !.ACC ATOMIC statement" }
  i = i + 1
  v = i

  !$acc atomic update capture capture  ! { dg-error "23: Unexpected junk after !.ACC ATOMIC statement" }
  i = i + 1
  v = i

  !$acc atomic write capture  ! { dg-error "22: Unexpected junk after !.ACC ATOMIC statement" }
  i = 1

  ! Valid in C/C++ since OpenACC 2.5 but not in Fortran:
  !$acc atomic update capture  ! { dg-error "23: Unexpected junk after !.ACC ATOMIC statement" }
  i = i + 1
  v = i
end
