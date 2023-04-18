! { dg-do run }
!
! Test the fix for PR88247 and more besides :-)
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      character(:), allocatable :: c
      character(:), dimension(:), allocatable :: d
   end type
   type(t), allocatable :: x

   call foo ('abcdef','ghijkl')
   associate (y => [x%c(:)])
      if (y(1) .ne. 'abcdef') stop 1
   end associate

   call foo ('ghi','ghi')
   associate (y => [x%c(2:)])
      if (y(1) .ne. 'hi') stop 2
   end associate

   call foo ('lmnopq','ghijkl')
   associate (y => [x%c(:3)])
      if (y(1) .ne. 'lmn') stop 3
   end associate

   call foo ('abcdef','ghijkl')
   associate (y => [x%c(2:4)])
      if (y(1) .ne. 'bcd') stop 4
   end associate

   call foo ('lmnopqrst','ghijklmno')
   associate (y => x%d(:))
      if (len(y) .ne. 9) stop 5
      if (any (y .ne. ['lmnopqrst','ghijklmno'])) stop 5
      y(1) = 'zqrtyd'
   end associate
   if (x%d(1) .ne. 'zqrtyd') stop 5

   call foo ('lmnopqrst','ghijklmno')
   associate (y => x%d(:)(2:4))
      if (any (y .ne. ['mno','hij'])) stop 6
   end associate

   call foo ('abcdef','ghijkl')
   associate (y => [x%d(:)])
      if (len(y) .ne. 6) stop 7
      if (any (y .ne. ['abcdef','ghijkl'])) stop 7
   end associate

   call foo ('lmnopqrst','ghijklmno')
   associate (y => [x%d(2:1:-1)])
      if (len(y) .ne. 9) stop 8
      if (any (y .ne. ['ghijklmno','lmnopqrst'])) stop 8
   end associate

   deallocate (x)
contains
   subroutine foo (c1, c2)
     character(*) :: c1, c2
     if (allocated (x)) deallocate (x)
     allocate (x)
     x%c = c1
     x%d = [c1, c2]
   end subroutine foo
end
