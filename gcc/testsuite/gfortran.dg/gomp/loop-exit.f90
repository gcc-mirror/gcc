subroutine sub1
!$omp do
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub2
!$omp parallel do
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub3
!$omp simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub4
!$omp do simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub5
!$omp parallel do simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub6
!$omp distribute simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub7
!$omp distribute parallel do
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub8
!$omp distribute parallel do simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub9
!$omp teams distribute simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub10
!$omp target teams distribute simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub11
!$omp teams distribute parallel do
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub12
!$omp target teams distribute parallel do
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub13
!$omp teams distribute parallel do simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub14
!$omp target teams distribute parallel do simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub15
!$omp target parallel do
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub16
!$omp target parallel do simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub17
!$omp target simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub18
!$omp taskloop simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub19
!$omp parallel master taskloop simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub20
!$omp master taskloop simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub21
!$omp parallel masked taskloop simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub22
!$omp masked taskloop simd
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub23
!$omp loop
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub24
!$omp parallel loop
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub25
!$omp teams loop
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub26
!$omp target parallel loop
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub27
!$omp target teams loop
outer: do i = 1, 5
  inner: do j = 1, 5
       if (k == 5) exit  ! ok
       if (k == 7) exit inner  ! ok
       if (k == 9) exit outer  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do inner
end do outer
end

subroutine sub28
!$omp do collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub29
!$omp parallel do collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub30
!$omp simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub31
!$omp do simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub32
!$omp parallel do simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub33
!$omp distribute simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub34
!$omp distribute parallel do collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub35
!$omp distribute parallel do simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub36
!$omp teams distribute simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub37
!$omp target teams distribute simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub38
!$omp teams distribute parallel do collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub39
!$omp target teams distribute parallel do collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub40
!$omp teams distribute parallel do simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub41
!$omp target teams distribute parallel do simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub42
!$omp target parallel do collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub43
!$omp target parallel do simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub44
!$omp target simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub45
!$omp taskloop simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub46
!$omp parallel master taskloop simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub47
!$omp master taskloop simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub48
!$omp parallel masked taskloop simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub49
!$omp masked taskloop simd collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub50
!$omp loop collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub51
!$omp parallel loop collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub52
!$omp teams loop collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub53
!$omp target parallel loop collapse(3)
do ii = i1, i2
do jj = j1, j2
do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end

subroutine sub54
!$omp target teams loop collapse(3)
do ii = i1, i2
 do jj = j1, j2
   do kk = k1, k2
     if (kk > 5) then
       k = 0
     end if
     if (kk == 7) exit  ! { dg-error "EXIT statement at .1. terminating !.OMP DO loop" }
  end do
  end do
end do
end
