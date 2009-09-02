! { dg-do compile }
! { dg-options "-std=gnu -ffixed-form -Wline-truncation" }
! PR39229 No warning of truncated lines if a continuation line follows 
      ! expected: no warning by default (as column 73+ is often used for )
      ! comments in fixed-form source code.
      ! however, with -wline-truncation there shall be a warning.
      implicit none
        call foo([11, 22, 33, 44, 55, 66, 770, 9900, 1100, 1100, 120], 12 warn
     &          , 'hello')
      print *, min(35 
     1                                        ,                        25 warn
     2  )
      contains
      subroutine foo(a,n,s)
        integer :: a(*), n, i
        character(len=*) :: s
        do i = 1, n
          print *, s, a(i)
        end do
      end subroutine foo
      end
! { dg-warning "Line truncated" " " { target *-*-* } 8 }
! { dg-warning "Line truncated" " " { target *-*-* } 11 }
