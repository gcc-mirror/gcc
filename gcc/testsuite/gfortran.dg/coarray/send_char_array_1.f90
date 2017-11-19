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
  co_str_k1_scal[1] = str_k1_scal
  if (co_str_k1_scal /= str_k1_scal // '          ') call abort()

  co_str_k4_scal[1] = str_k4_scal
  if (co_str_k4_scal /= str_k4_scal // 4_'          ') call abort()

  co_str_k4_scal[1] = str_k1_scal
  if (co_str_k4_scal /= str_k4_scal // 4_'          ') call abort()

  co_str_k1_scal[1] = str_k4_scal
  if (co_str_k1_scal /= str_k1_scal // '          ') call abort()

  co_str_k1_arr(:)[1] = str_k1_arr
  if (any(co_str_k1_arr /= ['abc  ', 'EFG  ', 'klm  ', 'NOP  '])) call abort()
 
  co_str_k4_arr(:)[1] = [4_'abc', 4_'EFG', 4_'klm', 4_'NOP']! str_k4_arr
  if (any(co_str_k4_arr /= [4_'abc  ', 4_'EFG  ', 4_'klm  ', 4_'NOP  '])) call abort()

  co_str_k4_arr(:)[1] = str_k1_arr
  if (any(co_str_k4_arr /= [ 4_'abc  ', 4_'EFG  ', 4_'klm  ', 4_'NOP  '])) call abort()

  co_str_k1_arr(:)[1] = str_k4_arr
  if (any(co_str_k1_arr /= ['abc  ', 'EFG  ', 'klm  ', 'NOP  '])) call abort()

end program send_convert_char_array

! vim:ts=2:sts=2:sw=2:
