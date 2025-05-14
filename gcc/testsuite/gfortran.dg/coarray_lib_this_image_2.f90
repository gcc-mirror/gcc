! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
!

  implicit none
  real :: x(2)[*]
  call bar(x)
contains
  subroutine bar(x)
    integer :: mylcobound, myucobound, mylbound, mythis_image
    real :: x(:)[5:*]
    mylcobound = lcobound(x,dim=1)
    myucobound = ucobound(x,dim=1)
    mylbound = lbound(x,dim=1)
    mythis_image = this_image()
  end subroutine bar
end

! { dg-final { scan-tree-dump-times "bar \\(struct array02_real\\(kind=4\\) & restrict x, void \\* restrict caf_token.., integer\\(kind=\[48\]\\) caf_offset..\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "mylcobound = 5;" 1 "original" } }
! { dg-final { scan-tree-dump-times "parm...dim\\\[1\\\].lbound = 5;" 1 "original" } }
! { dg-final { scan-tree-dump-times "myucobound =\[^\n\r\]* parm...dim\\\[1\\\].lbound \\+ \[^\n\r\]*_gfortran_caf_num_images \\(0B, 0B\\).? \\+ -?\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "mylbound = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "mythis_image = _gfortran_caf_this_image \\(0B\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "bar \\(&parm.\[0-9\]+, caf_token.\[0-9\]+, \\(integer\\(kind=\[48\]\\)\\) parm.\[0-9\]+.data - \\(integer\\(kind=\[48\]\\)\\) x\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_init \\(&argc, &argv\\);" 1 "original" } }
