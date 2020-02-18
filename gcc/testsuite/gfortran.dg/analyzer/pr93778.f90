program h0
  type bl
     integer jq
  end type bl
  type qn
     type (bl), dimension(3) :: xi
  end type qn
  type (qn) ro
  namelist /i2/ ro
  read(10, nml = i2)
end program h0
