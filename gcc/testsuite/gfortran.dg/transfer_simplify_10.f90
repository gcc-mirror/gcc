! { dg-do run }
!
! PR fortran/46638
!
! Contributed by James Van Buskirk
!
program test5
   use ISO_C_BINDING
   implicit none
   type, bind(C) :: CPUID_type
      integer(C_INT32_T) eax
      integer(C_INT32_T) ebx
      integer(C_INT32_T) edx
      integer(C_INT32_T) ecx
      integer(C_INT32_T) bbb
   end type CPUID_type
   type(CPUID_TYPE) result
  result = transfer(achar(10)//achar(0)//achar(0)//achar(0)//'GenuineIntel'//'abcd',result)

  if((     int(z'0000000A') /= result%eax  &
      .or. int(z'756E6547') /= result%ebx  &
      .or. int(z'49656E69') /= result%edx  &
      .or. int(z'6C65746E') /= result%ecx  &
      .or. int(z'64636261') /= result%bbb) &
     .and. & ! Big endian
     (     int(z'0A000000') /= result%eax  &
      .or. int(z'47656E75') /= result%ebx  &
      .or. int(z'696E6549') /= result%edx  &
      .or. int(z'6E74656C') /= result%ecx  &
      .or. int(z'61626364') /= result%bbb)) then
    write(*,'(5(z8.8:1x))') result
    STOP 1
  end if
end program test5 
