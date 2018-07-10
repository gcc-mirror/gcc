! PR fortran/85703
! { dg-do compile }

character function f()
  !$omp single
  !$omp end single
  f = 'a'
end
