!{ dg-do run }
 
program send_convert_char_array

  implicit none

  character(kind=1, len=:), allocatable, codimension[:] :: co_str_k1_scal
  character(kind=1, len=:), allocatable :: str_k1_scal
  character(kind=4, len=:), allocatable, codimension[:] :: co_str_k4_scal
  character(kind=4, len=:), allocatable :: str_k4_scal

  character(kind=1, len=:), allocatable, codimension[:] :: co_str_k1_arr(:)
  character(kind=1, len=:), allocatable :: str_k1_arr(:)
  character(kind=4, len=:), allocatable, codimension[:] :: co_str_k4_arr(:)
  character(kind=4, len=:), allocatable :: str_k4_arr(:)

  allocate(str_k1_scal, SOURCE='abcdefghij')
  allocate(str_k4_scal, SOURCE=4_'abcdefghij')
  allocate(character(len=20)::co_str_k1_scal[*]) ! allocate syncs here
  allocate(character(kind=4, len=20)::co_str_k4_scal[*]) ! allocate syncs here

  allocate(str_k1_arr, SOURCE=['abc', 'EFG', 'klm', 'NOP'])
  allocate(str_k4_arr, SOURCE=[4_'abc', 4_'EFG', 4_'klm', 4_'NOP'])
  allocate(character(len=5)::co_str_k1_arr(4)[*])
  allocate(character(kind=4, len=5)::co_str_k4_arr(4)[*])

  ! First check send/copy to self
  co_str_k1_scal[this_image()] = str_k1_scal
  if (co_str_k1_scal /= str_k1_scal // '          ') STOP 1

  co_str_k4_scal[this_image()] = str_k4_scal
  if (co_str_k4_scal /= str_k4_scal // 4_'          ') STOP 2

  co_str_k4_scal[this_image()] = str_k1_scal
  if (co_str_k4_scal /= str_k4_scal // 4_'          ') STOP 3

  co_str_k1_scal[this_image()] = str_k4_scal
  if (co_str_k1_scal /= str_k1_scal // '          ') STOP 4

  co_str_k1_arr(:)[this_image()] = str_k1_arr
  if (any(co_str_k1_arr /= ['abc  ', 'EFG  ', 'klm  ', 'NOP  '])) STOP 5

  co_str_k4_arr(:)[this_image()] = str_k4_arr
  if (any(co_str_k4_arr /= [4_'abc  ', 4_'EFG  ', 4_'klm  ', 4_'NOP  '])) STOP 6
 
  co_str_k4_arr(:)[this_image()] = str_k1_arr
  if (any(co_str_k4_arr /= [ 4_'abc  ', 4_'EFG  ', 4_'klm  ', 4_'NOP  '])) STOP 7

  co_str_k1_arr(:)[this_image()] = str_k4_arr
  if (any(co_str_k1_arr /= ['abc  ', 'EFG  ', 'klm  ', 'NOP  '])) STOP 8

  co_str_k1_arr(:)[this_image()] = ['abc', 'EFG', 'klm', 'NOP']
  if (any(co_str_k1_arr /= ['abc  ', 'EFG  ', 'klm  ', 'NOP  '])) STOP 9

  co_str_k4_arr(:)[this_image()] = [4_'abc', 4_'EFG', 4_'klm', 4_'NOP']
  if (any(co_str_k4_arr /= [4_'abc  ', 4_'EFG  ', 4_'klm  ', 4_'NOP  '])) STOP 10

end program send_convert_char_array

