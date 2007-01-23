program test_convert

  implicit none
  character(len=4)              :: byte_string
  character(len=1),dimension(4) :: byte_array
  integer*4 :: value,value1,n,i

  byte_string(1:1) = char(157)
  byte_string(2:2) = char(127)
  byte_string(3:3) = char(100)
  byte_string(4:4) = char(0)

  byte_array(1:4) = (/char(157),char(127),char(100),char(0)/)

  value = transfer(byte_string(1:4),value)
  value1 = transfer(byte_array(1:4),value1)

  if (value .ne. value1)  call abort()
end program test_convert
