! { dg-do compile }

module declare_target_1
  !$omp declare target to (var_1, var_4) link (var_2, var_3) &
  !$omp & link (var_5) to (var_6)
  integer :: var_1, var_2, var_3, var_4, var_5, var_6
  interface
    subroutine foo
      !$omp declare target
    end subroutine
  end interface
end
subroutine bar
  !$omp declare target
  integer, save :: var_9
  !$omp declare target link (var_8) to (baz, var_7) link (var_9) to (var_10)
  integer, save :: var_7, var_8, var_10
  integer :: var_11, var_12, var_13, var_14
  common /c1/ var_11, var_12
  common /c2/ var_13
  common /c3/ var_14
  !$omp declare target (baz, var_7, var_10, /c1/)
  !$omp declare target to (/c2/)
  !$omp declare target link (/c3/)
  !$omp declare target (bar)
  call baz
end subroutine
