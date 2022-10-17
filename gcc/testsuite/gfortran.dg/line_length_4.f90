! { dg-do compile }
! { dg-options "-Wline-truncation -ffree-line-length-80" }
! PR39229 No warning of truncated lines if a continuation line follows 
  implicit none
  call foo([11, 22, 33, 44, 55, 66, 770, 9900, 1100, 1100, 120],11,'hello') !no warn

  print *, min(35 &
   &  ,                        25  ), " Explanation ! "                         warn
  contains
   subroutine foo(a,n,s)
     integer :: a(*), n, i
     character(len=*) :: s
     do i = 1, n
       print *, s, a(i)
     end do
   end subroutine foo
  end
! { dg-error "Line truncated" " " { target *-*-* } 8 }
! { dg-prune-output "some warnings being treated as errors" }
