! { dg-additional-options "-fdump-tree-original" }

subroutine foo (a, n)
  integer :: n, a(:)
  integer :: i, b(n), c(n)
  !!$acc declare copyin(readonly: a(:), b(:n)) copyin(c(:))
  !$acc declare copyin(readonly: b) copyin(c)

  !$acc parallel copyin(readonly: a(:), b(:n)) copyin(c(:))
  do i = 1,32
     !$acc cache (readonly: a(:), b(:n))
     !$acc cache (c(:))
  enddo
  !$acc end parallel

  !$acc kernels copyin(readonly: a(:), b(:n)) copyin(c(:))
  do i = 1,32
     !$acc cache (readonly: a(:), b(:n))
     !$acc cache (c(:))
  enddo
  !$acc end kernels

  !$acc serial copyin(readonly: a(:), b(:n)) copyin(c(:))
  do i = 1,32
     !$acc cache (readonly: a(:), b(:n))
     !$acc cache (c(:))
  enddo
  !$acc end serial

  !$acc data copyin(readonly: a(:), b(:n)) copyin(c(:))
  do i = 1,32
     !$acc cache (readonly: a(:), b(:n))
     !$acc cache (c(:))
  enddo
  !$acc end data

  !$acc enter data copyin(readonly: a(:), b(:n)) copyin(c(:))

end subroutine foo

program main
  integer :: g(32), h(32)
  integer :: i, n = 32, a(32)
  integer :: b(32), c(32)

  !$acc declare copyin(readonly: g), copyin(h)

  !$acc parallel copyin(readonly: a(:32), b(:n)) copyin(c(:))
  do i = 1,32
     !$acc cache (readonly: a(:), b(:n))
     !$acc cache (c(:))
  enddo
  !$acc end parallel

  !$acc kernels copyin(readonly: a(:), b(:n)) copyin(c(:))
  do i = 1,32
     !$acc cache (readonly: a(:), b(:n))
     !$acc cache (c(:))
  enddo
  !$acc end kernels

  !$acc serial copyin(readonly: a(:), b(:n)) copyin(c(:))
  do i = 1,32
     !$acc cache (readonly: a(:), b(:n))
     !$acc cache (c(:))
  enddo
  !$acc end serial

  !$acc data copyin(readonly: a(:), b(:n)) copyin(c(:))
  do i = 1,32
     !$acc cache (readonly: a(:), b(:n))
     !$acc cache (c(:))
  enddo
  !$acc end data

  !$acc enter data copyin(readonly: a(:), b(:n)) copyin(c(:))

end program main

! The front end turns OpenACC 'declare' into OpenACC 'data'.
!   { dg-final { scan-tree-dump-times "(?n)#pragma acc data map\\(readonly,to:\\*b\\) map\\(alloc:b.+ map\\(to:\\*c\\) map\\(alloc:c.+" 1 "original" } }
!   { dg-final { scan-tree-dump-times "(?n)#pragma acc data map\\(readonly,to:g\\) map\\(to:h\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel map\\(readonly,to:\\*.+ map\\(alloc:a.+ map\\(readonly,to:\\*.+ map\\(alloc:b.+ map\\(to:\\*.+ map\\(alloc:c.+" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel map\\(readonly,to:a.+ map\\(alloc:a.+ map\\(readonly,to:b.+ map\\(alloc:b.+ map\\(to:c.+ map\\(alloc:c.+" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc kernels map\\(readonly,to:\\*.+ map\\(alloc:a.+ map\\(readonly,to:\\*.+ map\\(alloc:b.+ map\\(to:\\*.+ map\\(alloc:c.+" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc kernels map\\(readonly,to:a.+ map\\(alloc:a.+ map\\(readonly,to:b.+ map\\(alloc:b.+ map\\(to:c.+ map\\(alloc:c.+" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc serial map\\(readonly,to:\\*.+ map\\(alloc:a.+ map\\(readonly,to:\\*.+ map\\(alloc:b.+ map\\(to:\\*.+ map\\(alloc:c.+" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc serial map\\(readonly,to:a.+ map\\(alloc:a.+ map\\(readonly,to:b.+ map\\(alloc:b.+ map\\(to:c.+ map\\(alloc:c.+" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc data map\\(readonly,to:\\*.+ map\\(alloc:a.+ map\\(readonly,to:\\*.+ map\\(alloc:b.+ map\\(to:\\*.+ map\\(alloc:c.+" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc data map\\(readonly,to:a.+ map\\(alloc:a.+ map\\(readonly,to:b.+ map\\(alloc:b.+ map\\(to:c.+ map\\(alloc:c.+" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc enter data map\\(readonly,to:\\*.+ map\\(alloc:a.+ map\\(readonly,to:\\*.+ map\\(alloc:b.+ map\\(to:\\*.+ map\\(alloc:c.+" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc enter data map\\(readonly,to:a.+ map\\(alloc:a.+ map\\(readonly,to:b.+ map\\(alloc:b.+ map\\(to:c.+ map\\(alloc:c.+" 1 "original" } }

! { dg-final { scan-tree-dump-times "(?n)#pragma acc cache \\(readonly:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) parm.*data \\\[len: .+\\\]\\) \\(readonly:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) parm.*data \\\[len: .+\\\]\\);" 8 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma acc cache \\(\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) parm.*data \\\[len: .+\\\]\\);" 8 "original" } }
