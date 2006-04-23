! { dg-do compile }
! This tests the patch for PR26787 in which it was found that setting
! the result of one module procedure from within another produced an
! ICE rather than an error.
!
! This is an "elaborated" version of the original testcase from
! Joshua Cogliati  <jjcogliati-r1@yahoo.com>
!
function ext1 ()
    integer ext1, ext2, arg
    ext1 = 1
    entry ext2 (arg)
    ext2 = arg
! gcc-4.2 version contains this:
!contains
!    subroutine int_1 ()
!        ext1 = arg * arg     ! OK - host associated.
!    end subroutine int_1
end function ext1

module simple
    implicit none
contains
    integer function foo () 
         foo = 10            ! OK - function result
         call foobar ()
    contains
        subroutine foobar ()
            integer z
            foo = 20         ! OK - host associated.
        end subroutine foobar
    end function foo
    subroutine bar()         ! This was the original bug.
        foo = 10             ! { dg-error "is not a VALUE" }
    end subroutine bar
    integer function oh_no ()
        oh_no = 1
        foo = 5              ! { dg-error "is not a VALUE" }
    end function oh_no
end module simple

module simpler
    implicit none
contains
    integer function foo_er () 
         foo_er = 10         ! OK - function result
    end function foo_er
end module simpler

    use simpler
    real w, stmt_fcn
    interface
	function ext1 ()
	    integer ext1
	end function ext1
	function ext2 (arg)
	    integer ext2, arg
	end function ext2
    end interface
    stmt_fcn (w) = sin (w)     
    call x (y ())
    x = 10                   ! { dg-error "Expected VARIABLE" }
    y = 20                   ! { dg-error "is not a VALUE" }
    foo_er = 8               ! { dg-error "is not a VALUE" }
    ext1 = 99                ! { dg-error "is not a VALUE" }
    ext2 = 99                ! { dg-error "is not a VALUE" }
    stmt_fcn = 1.0           ! { dg-error "Expected VARIABLE" }
    w = stmt_fcn (1.0)
contains
    subroutine x (i)
        integer i
        y = i                ! { dg-error "is not a VALUE" }
    end subroutine x
    function y ()
        integer y
        y = 2                ! OK - function result
    end function y
end
! { dg-final { cleanup-modules "simple simpler" } }