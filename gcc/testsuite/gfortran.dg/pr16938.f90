! { dg-do run }
! We used to get an internal error in the backend when trying to compile this
! Added some code which verifies that we're actually doing the right thing.
  program Array_List 
    implicit none 
 
    type :: Compound 
      integer                       :: Count 
      character (len = 4)           :: Name 
    end type Compound 

    type :: Table 
      type (Compound)               :: Data (2)  
      integer :: L_Size  
    end type Table 
 
    type (Table) :: ElementTable
    ElementTable%Data(1) = Compound(1,"one")
    ElementTable%Data(2) = Compound(2,"two")
    ElementTable%L_size  = 2 

    if (elementtable%data(1)%count /= 1) call abort
    if (elementtable%data(2)%count /= 2) call abort
    if (elementtable%data(1)%name /= "one ") call abort
    if (elementtable%data(2)%name /= "two ") call abort
    if (elementtable%l_size /= 2) call abort
  end program Array_List
