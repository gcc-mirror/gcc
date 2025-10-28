! { dg-do compile }
! { dg-additional-options "-Wunused-label" }

! Check that a statement label defined in the first statement past a
! metadirective body is bound to the outer region.


integer :: cnt, x

cnt = 0
!$omp begin metadirective when(user={condition(cnt > 0)} : parallel)
  x = 5
!$omp end metadirective
1234 format("Hello")
write(*,1234)

!$omp begin metadirective when(user={condition(x > 0)} : parallel)
  x = 5
!$omp end metadirective
4567 print *, 'hello', cnt
cnt = cnt + 1
if (cnt < 2) goto 4567
end
