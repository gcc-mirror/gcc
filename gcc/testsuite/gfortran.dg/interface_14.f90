! { dg-do compile }
! Checks the fix for a regression PR32526, which was caused by
! the patch for PR31494.  The problem here was that the symbol
! 'new' was determined to be ambiguous.
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
    module P_Class
      implicit none
      private :: init_Personnel
      interface new
         module procedure init_Personnel
      end interface
      contains
         subroutine init_Personnel(this)
         integer, intent (in) :: this
         print *, "init personnel", this
         end subroutine init_Personnel
    end module P_Class

    module S_Class
      use P_Class
      implicit none
      private :: init_Student
      type Student
         private
         integer :: personnel = 1
      end type Student
      interface new
         module procedure init_Student
      end interface
      contains
         subroutine init_Student(this)
         type (Student), intent (in) :: this
         call new(this%personnel)
         end subroutine init_Student
    end module S_Class

    module T_Class
      use P_Class
      implicit none
      private :: init_Teacher
      type Teacher
         private
         integer :: personnel = 2
      end type Teacher
      interface new
         module procedure init_Teacher
      end interface
      contains
         subroutine init_Teacher(this)
         type (Teacher), intent (in) :: this
         call new(this%personnel)
         end subroutine init_Teacher
    end module T_Class

    module poly_Class
      use S_Class
      use T_Class
    end module poly_Class

    module D_Class
      use poly_Class
    end module D_Class

      use D_Class
      type (Teacher) :: a
      type (Student) :: b
      call new (a)
      call new (b)
      end

! { dg-final { cleanup-modules "P_class S_Class T_Class D_Class poly_Class" } }
