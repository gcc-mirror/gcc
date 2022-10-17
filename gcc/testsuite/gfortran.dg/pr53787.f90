! { dg-do compile }
! { dg-options "-O3 -fdump-ipa-cp-details -fno-ipa-sra -fno-inline -fwhole-program" }

  real x(10)
  n = 10
  call init(x,n)
  print *, x
end program

subroutine init(x, n)
  real x(10)
  do i=1,n
     x(i) = i*i + 1
  enddo

  return
end subroutine init

! { dg-final { scan-ipa-dump "Creating a specialized node of init" "cp" } }
! { dg-final { scan-ipa-dump-times "Aggregate replacements" 2 "cp" } }
