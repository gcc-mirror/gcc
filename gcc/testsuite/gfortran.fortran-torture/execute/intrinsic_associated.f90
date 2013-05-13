! Program to test the ASSOCIATED intrinsic.
program intrinsic_associated
   call pointer_to_section ()
   call associate_1 ()
   call pointer_to_derived_1 ()
   call associated_2 ()
end

subroutine pointer_to_section ()
   integer, dimension(5, 5), target :: xy
   integer, dimension(:, :), pointer :: window
   data xy /25*0/
   logical t

   window => xy(2:4, 3:4)
   window = 10
   window (1, 1) = 0101
   window (3, 2) = 4161
   window (3, 1) = 4101
   window (1, 2) = 0161

   t = associated (window, xy(2:4, 3:4))
   if (.not.t) call abort ()
   ! Check that none of the array got mangled
   if ((xy(2, 3) .ne. 0101) .or. (xy (4, 4) .ne. 4161) &
       .or. (xy(4, 3) .ne. 4101) .or. (xy (2, 4) .ne. 0161)) call abort ()
   if (any (xy(:, 1:2) .ne. 0)) call abort ()
   if (any (xy(:, 5) .ne. 0)) call abort ()
   if (any (xy (1, 3:4) .ne. 0)) call abort ()
   if (any (xy (5, 3:4) .ne. 0)) call abort ()
   if (xy(3, 3) .ne. 10) call abort ()
   if (xy(3, 4) .ne. 10) call abort ()
   if (any (xy(2:4, 3:4) .ne. window)) call abort ()
end

subroutine sub1 (a, ap)
   integer, pointer :: ap(:, :)
   integer, target :: a(10, 10)

   ap => a
end

subroutine nullify_pp (a)
   integer, pointer :: a(:, :)

   if (.not. associated (a)) call abort ()
   nullify (a)
end

subroutine associate_1 ()
   integer, pointer :: a(:, :), b(:, :)
   interface 
      subroutine nullify_pp (a)
         integer, pointer :: a(:, :)
      end subroutine nullify_pp
   end interface

   allocate (a(80, 80))
   b => a
   if (.not. associated(a)) call abort ()
   if (.not. associated(b)) call abort ()
   call nullify_pp (a)
   if (associated (a)) call abort ()
   if (.not. associated (b)) call abort ()
end

subroutine pointer_to_derived_1 ()
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

   nullify (r1%r1p, r2%r2p, e1%rp, e2%rp, e3%rp)
   if (associated (r1%r1p)) call abort ()
   if (associated (r2%r2p)) call abort ()
   if (associated (e2%rp)) call abort ()
   if (associated (e1%rp)) call abort ()
   if (associated (e3%rp)) call abort ()
   r1%r1p => r2
   r2%r2p => r1
   r1%value = 11
   r2%value = 22
   e1%rp => e2
   e2%rp => e3
   e1%value = 33
   e1%rp%value = 44
   e1%rp%rp%value = 55
   if (.not. associated (r1%r1p)) call abort ()
   if (.not. associated (r2%r2p)) call abort ()
   if (.not. associated (e1%rp)) call abort ()
   if (.not. associated (e2%rp)) call abort ()
   if (associated (e3%rp)) call abort ()
   if (r1%r1p%value .ne. 22) call abort ()
   if (r2%r2p%value .ne. 11) call abort ()
   if (e1%value .ne. 33) call abort ()
   if (e2%value .ne. 44) call abort ()
   if (e3%value .ne. 55) call abort ()
   if (r1%value .ne. 11) call abort ()
   if (r2%value .ne. 22) call abort ()

end 

subroutine associated_2 ()
   integer, pointer :: xp(:, :)
   integer, target  :: x(10, 10)
   integer, target  :: y(100, 100)
   interface
      subroutine sub1 (a, ap)
         integer, pointer :: ap(:, :)
         integer, target  :: a(10, 10)
      end
   endinterface

   xp => y
   if (.not. associated (xp)) call abort ()
   call sub1 (x, xp)
   if (associated (xp, y)) call abort ()
   if (.not. associated (xp, x)) call abort ()
end

