! { dg-do compile }
! { dg-options "-std=f95 -fmax-errors=100" }
!
! PR fortran/34665
!
! Test argument checking
!
! TODO: Check also expressions, e.g. "(a(1))" instead of "a(1)
! for strings; check also "string" and [ "string" ]
!
implicit none
CONTAINS
SUBROUTINE test1(a,b,c,d,e)
 integer, dimension(:) :: a
 integer, pointer, dimension(:) :: b
 integer, dimension(*) :: c
 integer, dimension(5) :: d
 integer               :: e

 call as_size(a)
 call as_size(b)
 call as_size(c)
 call as_size(d)
 call as_size(e) ! { dg-error "Rank mismatch" }
 call as_size(1) ! { dg-error "Rank mismatch" }
 call as_size( (/ 1 /) )
 call as_size( (a) )
 call as_size( (b) )
 call as_size( (c) ) ! { dg-error "The upper bound in the last dimension must appear in the reference to the assumed size array" }
 call as_size( (d) )
 call as_size( (e) ) ! { dg-error "Rank mismatch" }
 call as_size(a(1)) ! { dg-error "Element of assumed-shape" }
 call as_size(b(1)) ! { dg-error "Element of assumed-shape" }
 call as_size(c(1))
 call as_size(d(1))
 call as_size( (a(1)) ) ! { dg-error "Rank mismatch" }
 call as_size( (b(1)) ) ! { dg-error "Rank mismatch" }
 call as_size( (c(1)) ) ! { dg-error "Rank mismatch" }
 call as_size( (d(1)) ) ! { dg-error "Rank mismatch" }
 call as_size(a(1:2))
 call as_size(b(1:2))
 call as_size(c(1:2))
 call as_size(d(1:2))
 call as_size( (a(1:2)) )
 call as_size( (b(1:2)) )
 call as_size( (c(1:2)) )
 call as_size( (d(1:2)) )

 call as_shape(a)
 call as_shape(b)
 call as_shape(c) ! { dg-error "cannot be an assumed-size array" }
 call as_shape(d)
 call as_shape(e) ! { dg-error "Rank mismatch" }
 call as_shape( 1 ) ! { dg-error "Rank mismatch" }
 call as_shape( (/ 1 /) )
 call as_shape( (a) )
 call as_shape( (b) )
 call as_shape( (c) ) ! { dg-error "The upper bound in the last dimension must appear in the reference to the assumed size array" }
 call as_shape( (d) )
 call as_shape( (e) ) ! { dg-error "Rank mismatch" }
 call as_shape( (1) ) ! { dg-error "Rank mismatch" }
 call as_shape( ((/ 1 /)) )
 call as_shape(a(1)) ! { dg-error "Rank mismatch" }
 call as_shape(b(1)) ! { dg-error "Rank mismatch" }
 call as_shape(c(1)) ! { dg-error "Rank mismatch" }
 call as_shape(d(1)) ! { dg-error "Rank mismatch" }
 call as_shape( (a(1)) ) ! { dg-error "Rank mismatch" }
 call as_shape( (b(1)) ) ! { dg-error "Rank mismatch" }
 call as_shape( (c(1)) ) ! { dg-error "Rank mismatch" }
 call as_shape( (d(1)) ) ! { dg-error "Rank mismatch" }
 call as_shape(a(1:2))
 call as_shape(b(1:2))
 call as_shape(c(1:2))
 call as_shape(d(1:2))
 call as_shape( (a(1:2)) )
 call as_shape( (b(1:2)) )
 call as_shape( (c(1:2)) )
 call as_shape( (d(1:2)) )

 call as_expl(a)
 call as_expl(b)
 call as_expl(c)
 call as_expl(d)
 call as_expl(e) ! { dg-error "Rank mismatch" }
 call as_expl( 1 ) ! { dg-error "Rank mismatch" }
 call as_expl( (/ 1, 2, 3 /) )
 call as_expl( (a) )
 call as_expl( (b) )
 call as_expl( (c) ) ! { dg-error "The upper bound in the last dimension must appear in the reference to the assumed size array" }
 call as_expl( (d) )
 call as_expl( (e) ) ! { dg-error "Rank mismatch" }
 call as_expl(a(1)) ! { dg-error "Element of assumed-shape" }
 call as_expl(b(1)) ! { dg-error "Element of assumed-shape" }
 call as_expl(c(1))
 call as_expl(d(1))
 call as_expl( (a(1)) ) ! { dg-error "Rank mismatch" }
 call as_expl( (b(1)) ) ! { dg-error "Rank mismatch" }
 call as_expl( (c(1)) ) ! { dg-error "Rank mismatch" }
 call as_expl( (d(1)) )  ! { dg-error "Rank mismatch" }
 call as_expl(a(1:3))
 call as_expl(b(1:3))
 call as_expl(c(1:3))
 call as_expl(d(1:3))
 call as_expl( (a(1:3)) )
 call as_expl( (b(1:3)) )
 call as_expl( (c(1:3)) )
 call as_expl( (d(1:3)) )
END SUBROUTINE test1

SUBROUTINE as_size(a)
 integer, dimension(*) :: a
END SUBROUTINE as_size

SUBROUTINE as_shape(a)
 integer, dimension(:) :: a
END SUBROUTINE as_shape

SUBROUTINE as_expl(a)
 integer, dimension(3) :: a
END SUBROUTINE as_expl


SUBROUTINE test2(a,b,c,d,e)
 character(len=*), dimension(:) :: a
 character(len=*), pointer, dimension(:) :: b
 character(len=*), dimension(*) :: c
 character(len=*), dimension(5) :: d
 character(len=*)               :: e

 call cas_size(a)
 call cas_size(b)
 call cas_size(c)
 call cas_size(d)
 call cas_size(e) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size("abc") ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size( (/"abc"/) )
 call cas_size(a//"a")
 call cas_size(b//"a")
 call cas_size(c//"a")  ! { dg-error "The upper bound in the last dimension must appear in the reference to the assumed size array" }
 call cas_size(d//"a")
 call cas_size(e//"a") ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size(("abc")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size( ((/"abc"/)) )
 call cas_size(a(1)) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size(b(1)) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size(c(1)) ! OK in F95
 call cas_size(d(1)) ! OK in F95
 call cas_size((a(1)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" } 
 call cas_size((b(1)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size((c(1)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" } 
 call cas_size((d(1)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size(a(1:2))
 call cas_size(b(1:2))
 call cas_size(c(1:2))
 call cas_size(d(1:2))
 call cas_size((a(1:2)//"a"))
 call cas_size((b(1:2)//"a"))
 call cas_size((c(1:2)//"a"))
 call cas_size((d(1:2)//"a"))
 call cas_size(a(:)(1:3))
 call cas_size(b(:)(1:3))
 call cas_size(d(:)(1:3))
 call cas_size((a(:)(1:3)//"a"))
 call cas_size((b(:)(1:3)//"a"))
 call cas_size((d(:)(1:3)//"a"))
 call cas_size(a(1:2)(1:3))
 call cas_size(b(1:2)(1:3))
 call cas_size(c(1:2)(1:3))
 call cas_size(d(1:2)(1:3))
 call cas_size((a(1:2)(1:3)//"a"))
 call cas_size((b(1:2)(1:3)//"a"))
 call cas_size((c(1:2)(1:3)//"a"))
 call cas_size((d(1:2)(1:3)//"a"))
 call cas_size(e(1:3)) ! { dg-error "Fortran 2003: Scalar CHARACTER" } 
 call cas_size("abcd"(1:3)) ! { dg-error "Fortran 2003: Scalar CHARACTER" } 
 call cas_size((e(1:3))) ! { dg-error "Fortran 2003: Scalar CHARACTER" } 
 call cas_size(("abcd"(1:3)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" } 

 call cas_shape(a)
 call cas_shape(b)
 call cas_shape(c) ! { dg-error "cannot be an assumed-size array" }
 call cas_shape(d)
 call cas_shape(e) ! { dg-error "Rank mismatch" }
 call cas_shape("abc") ! { dg-error "Rank mismatch" }
 call cas_shape( (/"abc"/) )
 call cas_shape(a//"c")
 call cas_shape(b//"c")
 call cas_shape(c//"c") ! { dg-error "The upper bound in the last dimension must appear in the reference to the assumed size array" }
 call cas_shape(d//"c")
 call cas_shape(e//"c") ! { dg-error "Rank mismatch" }
 call cas_shape(("abc")) ! { dg-error "Rank mismatch" }
 call cas_shape( ((/"abc"/)) )
 call cas_shape(a(1)) ! { dg-error "Rank mismatch" }
 call cas_shape(b(1)) ! { dg-error "Rank mismatch" }
 call cas_shape(c(1)) ! { dg-error "Rank mismatch" }
 call cas_shape(d(1)) ! { dg-error "Rank mismatch" }
 call cas_shape(a(1:2))
 call cas_shape(b(1:2))
 call cas_shape(c(1:2))
 call cas_shape(d(1:2))
 call cas_shape((a(1:2)//"a"))
 call cas_shape((b(1:2)//"a"))
 call cas_shape((c(1:2)//"a"))
 call cas_shape((d(1:2)//"a"))
 call cas_shape(a(:)(1:3))
 call cas_shape(b(:)(1:3))
 call cas_shape(d(:)(1:3))
 call cas_shape((a(:)(1:3)//"a"))
 call cas_shape((b(:)(1:3)//"a"))
 call cas_shape((d(:)(1:3)//"a"))
 call cas_shape(a(1:2)(1:3))
 call cas_shape(b(1:2)(1:3))
 call cas_shape(c(1:2)(1:3))
 call cas_shape(d(1:2)(1:3))
 call cas_shape((a(1:2)(1:3)//"a"))
 call cas_shape((b(1:2)(1:3)//"a"))
 call cas_shape((c(1:2)(1:3)//"a"))
 call cas_shape((d(1:2)(1:3)//"a"))
 call cas_size(e(1:3)) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size("abcd"(1:3)) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size((e(1:3))) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_size(("abcd"(1:3)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }

 call cas_expl(a)
 call cas_expl(b)
 call cas_expl(c)
 call cas_expl(d)
 call cas_expl(e) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl("abc") ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl((/"a","b","c"/))
 call cas_expl(a//"a")
 call cas_expl(b//"a")
 call cas_expl(c//"a")  ! { dg-error "The upper bound in the last dimension must appear in the reference to the assumed size array" }
 call cas_expl(d//"a")
 call cas_expl(e//"a") ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl(("abc")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl(((/"a","b","c"/)))
 call cas_expl(a(1)) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl(b(1)) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl(c(1)) ! OK in F95
 call cas_expl(d(1)) ! OK in F95
 call cas_expl((a(1)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl((b(1)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl((c(1)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl((d(1)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl(a(1:3))
 call cas_expl(b(1:3))
 call cas_expl(c(1:3))
 call cas_expl(d(1:3))
 call cas_expl((a(1:3)//"a"))
 call cas_expl((b(1:3)//"a"))
 call cas_expl((c(1:3)//"a"))
 call cas_expl((d(1:3)//"a"))
 call cas_expl(a(:)(1:3))
 call cas_expl(b(:)(1:3))
 call cas_expl(d(:)(1:3))
 call cas_expl((a(:)(1:3)))
 call cas_expl((b(:)(1:3)))
 call cas_expl((d(:)(1:3)))
 call cas_expl(a(1:2)(1:3))
 call cas_expl(b(1:2)(1:3))
 call cas_expl(c(1:2)(1:3))
 call cas_expl(d(1:2)(1:3))
 call cas_expl((a(1:2)(1:3)//"a"))
 call cas_expl((b(1:2)(1:3)//"a"))
 call cas_expl((c(1:2)(1:3)//"a"))
 call cas_expl((d(1:2)(1:3)//"a"))
 call cas_expl(e(1:3)) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl("abcd"(1:3)) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl((e(1:3))) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
 call cas_expl(("abcd"(1:3)//"a")) ! { dg-error "Fortran 2003: Scalar CHARACTER" }
END SUBROUTINE test2

SUBROUTINE cas_size(a)
 character(len=*), dimension(*) :: a
END SUBROUTINE cas_size

SUBROUTINE cas_shape(a)
 character(len=*), dimension(:) :: a
END SUBROUTINE cas_shape

SUBROUTINE cas_expl(a)
 character(len=*), dimension(3) :: a
END SUBROUTINE cas_expl
END
