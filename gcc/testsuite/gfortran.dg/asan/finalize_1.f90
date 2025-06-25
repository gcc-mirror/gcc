!{ dg-do run }

! PR fortran/120637

! Contributed by Antony Lewis  <antony@cosmologist.info>
! The unused module is needed to trigger the issue of not freeing the
! memory of second module.

    module MiscUtils
    implicit none

    contains
        
    logical function isFloat0(R)
    class(*), intent(in) :: R

    select type(R)
    type is (real)
        isFloat0 = .true.
    end select
    end function isFloat0

    end module MiscUtils
    
    module results3
    implicit none
    public

    Type ClTransferData2
      real, dimension(:,:,:), allocatable :: Delta_p_l_k
    end type ClTransferData2

    type TCLdata2
       Type(ClTransferData2) :: CTransScal, CTransTens, CTransVec
    end type TCLdata2 

    type :: CAMBdata2
        Type(TClData2) :: CLdata2
    end type  

    end module results3

program driver
   use results3
   integer i
   do i=1, 2
   call test()   
   end do

   contains

   subroutine test
       implicit none
       class(CAMBdata2), pointer :: Data

       allocate(CAMBdata2::Data)

       allocate(Data%ClData2%CTransScal%Delta_p_l_k(3, 1000, 1000)) 
       allocate(Data%ClData2%CTransVec%Delta_p_l_k(3, 1000, 1000))
       deallocate(Data)

   end subroutine test

 end program driver

!{ dg-final { cleanup-modules "miscutils results3" } }

