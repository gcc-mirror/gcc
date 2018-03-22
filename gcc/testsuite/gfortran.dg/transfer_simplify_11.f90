! { dg-do run }
! PR Fortran/82841
!
   integer, parameter :: N = 2
   character(len=1) :: chr(N)
   chr = transfer(repeat("x",ncopies=N),[character(len=1) ::], N)
   if (chr(1) /= 'x' .and. chr(2) /= 'x') STOP 1
end
