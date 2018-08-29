! Program to test DERIVED type with components point to the DERIVED
! type itself, and two DERIVED type with componets point to each 
! other.
program nest_derived
   type record
      integer :: value
      type(record), pointer :: rp
   end type record

   type record1
      integer value
      type(record2), pointer :: r1p
   end type

   type record2
      integer value
      type(record1), pointer :: r2p
   end type
            
   type(record), target :: e1, e2, e3 
   type(record1), target :: r1
   type(record2), target :: r2
   nullify(r1%r1p,r2%r2p,e1%rp,e2%rp,e3%rp)

   r1%r1p => r2
   r2%r2p => r1
   e1%rp => e2
   e2%rp => e3 

   r1%value = 11
   r2%value = 22

   e1%value = 33
   e1%rp%value = 44
   e1%rp%rp%value = 55

   if (r1%r1p%value .ne. 22) STOP 1
   if (r2%r2p%value .ne. 11) STOP 2
   if (e1%value .ne. 33) STOP 3
   if (e2%value .ne. 44) STOP 4
   if (e3%value .ne. 55) STOP 5
   if (r1%value .ne. 11) STOP 6
   if (r2%value .ne. 22) STOP 7

end
