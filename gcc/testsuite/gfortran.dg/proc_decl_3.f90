! { dg-do compile }
! Some tests for PROCEDURE declarations inside of interfaces.
! Contributed by Janus Weil <jaydub66@gmail.com>

module m

  interface
    subroutine a()
    end subroutine a
  end interface

  procedure(c) :: f

  interface bar
    procedure a,d
  end interface bar

  interface foo
    procedure c
  end interface foo

  abstract interface
    procedure f  ! { dg-error "must be in a generic interface" }
  end interface

  interface
    function opfoo(a)
      integer,intent(in) :: a
      integer :: opfoo
    end function opfoo
  end interface

  interface operator(.op.)
    procedure opfoo
  end interface

  external ex  ! { dg-error "has no explicit interface" }
  procedure():: ip  ! { dg-error "has no explicit interface" }
  procedure(real):: pip  ! { dg-error "has no explicit interface" }

  interface nn1
    procedure ex
    procedure a, a  ! { dg-error "already present in the interface" }
  end interface

  interface nn2
    procedure ip
  end interface

  interface nn3
    procedure pip
  end interface

contains

 subroutine d(x)

   interface
     subroutine x()
     end subroutine x
   end interface

   interface gen
     procedure x
   end interface

 end subroutine d

 function c(x)
   integer :: x
   real :: c
   c = 3.4*x
 end function c

end module m
