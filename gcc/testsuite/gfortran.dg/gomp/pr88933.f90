! PR ipa/88933
! { dg-do compile }
! { dg-options "-O1 -fexceptions -fipa-cp -fnon-call-exceptions -fopenmp -fno-inline-functions-called-once" }

!$omp parallel  
!$omp single
  call a
!$omp end single
!$omp end parallel
contains
  subroutine b (c, d, e, f, g, h, i, j, k, m)
    character (*) c
    character  d
    integer, dimension (m) :: e
    integer, dimension (m) :: f
    character  g
    character  h
    real, dimension (:, :, :) :: i
    double precision, dimension (:, :, :) :: j
    integer, dimension (:, :, :) :: k
     
    integer, dimension (m) :: l
!$omp task firstprivate (k) firstprivate (l)
    !$omp end task
  c = ''
  end  
  subroutine a
    character  c
    character  d
    integer, dimension (7) :: e
    integer, dimension (7) :: f
    character g
    character h
    real, dimension (5, 6, 7) :: i
    double precision, dimension (6, 6, 7) :: j
    integer, dimension (5, 7, 6) :: k
    call b (c, d, e, f, g, h, i, j, k, 7)
  end  
end
