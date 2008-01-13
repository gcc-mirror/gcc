! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/34665
!
! Test argument checking
!
implicit none
CONTAINS
SUBROUTINE test2(a,b,c,d,e)
 character(len=*), dimension(:) :: a
 character(len=*), pointer, dimension(:) :: b
 character(len=*), dimension(*) :: c
 character(len=*), dimension(5) :: d
 character(len=*)               :: e

 call cas_size(e) 
 call cas_size("abc") 
 call cas_size(e//"a") 
 call cas_size(("abc")) 
 call cas_size(a(1)) 
 call cas_size(b(1)) 
 call cas_size((a(1)//"a")) 
 call cas_size((b(1)//"a")) 
 call cas_size((c(1)//"a")) 
 call cas_size((d(1)//"a")) 
 call cas_size(e(1:3)) 
 call cas_size("abcd"(1:3)) 
 call cas_size((e(1:3))) 
 call cas_size(("abcd"(1:3)//"a")) 
 call cas_size(e(1:3)) 
 call cas_size("abcd"(1:3)) 
 call cas_size((e(1:3))) 
 call cas_size(("abcd"(1:3)//"a")) 
 call cas_expl(e) 
 call cas_expl("abc") 
 call cas_expl(e//"a") 
 call cas_expl(("abc")) 
 call cas_expl(a(1)) 
 call cas_expl(b(1)) 
 call cas_expl((a(1)//"a")) 
 call cas_expl((b(1)//"a")) 
 call cas_expl((c(1)//"a")) 
 call cas_expl((d(1)//"a")) 
 call cas_expl(e(1:3)) 
 call cas_expl("abcd"(1:3)) 
 call cas_expl((e(1:3))) 
 call cas_expl(("abcd"(1:3)//"a")) 
END SUBROUTINE test2

SUBROUTINE cas_size(a)
 character(len=*), dimension(*) :: a
END SUBROUTINE cas_size

SUBROUTINE cas_expl(a)
 character(len=*), dimension(5) :: a
END SUBROUTINE cas_expl
END

