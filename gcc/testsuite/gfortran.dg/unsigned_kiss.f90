!
! { dg-do run }
! { dg-options "-funsigned" }
!
! Modern Fortran rewrite of Marsaglia's 64-bit KISS PRNG.
! https://www.thecodingforums.com/threads/64-bit-kiss-rngs.673657/
!
module kissm

   implicit none
   private
   public uk, kseed, kiss

   integer, parameter :: uk = kind(1u_8)  ! Check kind() works.

   ! Default seeds.  Checks unsigned with parameter attribute.
   unsigned(uk), parameter :: seed(4) = [ &
   &  1234567890987654321u_uk, 362436362436362436u_uk, &
   &  1066149217761810u_uk, 123456123456123456u_uk ]

   ! Seeds used during generation
   unsigned(uk), save :: sd(4) = seed

   contains

      ! Tests unsigned in an internal function.
      function s(x)
         unsigned(uk) s
         unsigned(uk), intent(in) :: x
         s = ishft(x, -63)                ! Tests ishft
      end function

      ! Poor seeding routine.  Need to check v for entropy!
      ! Tests intent(in) and optional attributes.
      ! Tests ishftc() and array constructors.
      subroutine kseed(v)
         unsigned(uk), intent(in), optional :: v
         if (present(v)) then
            sd = seed + [ishftc(v,1), ishftc(v,15), ishftc(v,31), ishftc(v,44)]
         else
            sd = seed
         end if
      end subroutine kseed

      function kiss()
         unsigned(uk) kiss
         unsigned(uk) m, t
         integer k

         ! Test unsigned in a statement function
         m(t, k) = ieor(t, ishft(t, k))

         t = ishft(sd(1), 58) + sd(4)
         if (s(sd(1)) == s(t)) then
            sd(4) = ishft(sd(1), -6) + s(sd(1))
         else
            sd(4) = ishft(sd(1), -6) + 1u_uk - s(sd(1) + t)
         endif

         sd(1) = t + sd(1)
         sd(2) = m(m(m(sd(2), 13), -17), 43)
         sd(3) = 6906969069u_uk * sd(3) + 1234567u_uk
         kiss = sd(1) + sd(2) + sd(3)
      end function kiss
      
end module kissm

program testkiss
   use kissm
   integer, parameter :: n = 4
   unsigned(uk) prn(4)

   ! Default sequence
   unsigned(uk), parameter :: a(4) = [8932985056925012148u_uk, &
   &  5710300428094272059u_uk, 18342510866933518593u_uk,       &
   &  14303636270573868250u_uk]
   
   ! Sequence with the seed 123412341234u_uk
   unsigned(uk), parameter :: b(4) = [4002508872477953753u_uk, &
   &  18025327658415290923u_uk,  16058856976144281263u_uk,     &
   &  11842224026193909403u_uk]

   do i = 1, n
      prn(i) = kiss()
   end do
   if (any(prn /= a)) stop 1

   call kseed(123412341234u_uk)
   do i = 1, n
      prn(i) = kiss()
   end do
   if (any(prn /= b)) stop 2

   call kseed()
   do i = 1, n
      prn(i) = kiss()
   end do
   if (any(prn /= a)) stop 3

end program testkiss
