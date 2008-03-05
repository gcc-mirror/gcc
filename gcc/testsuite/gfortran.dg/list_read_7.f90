! { dg-do run { target fd_truncate } }
! PR33400 Formatted read fails if line ends without line break
! Test case modified from that in PR by <jvdelisle@gcc.gnu.org>
integer, parameter :: fgsl_strmax = 128
character(len=fgsl_strmax) :: ieee_str1, ieee_str2
open(unit=20, file='test.dat',form='FORMATTED', status="replace")
write(20,'(a)',advance="no") ' 1.01010101010101010101010101010101&
       &01010101010101010101*2^-2 1.01010101010101010101011*2^-2'
rewind(20)
read(20, fmt=*) ieee_str1, ieee_str2
if (trim(ieee_str1) /= &
    '1.0101010101010101010101010101010101010101010101010101*2^-2') &
  call abort
if (trim(ieee_str2) /= &
     '1.01010101010101010101011*2^-2') &
  call abort
close(20, status="delete")
end
