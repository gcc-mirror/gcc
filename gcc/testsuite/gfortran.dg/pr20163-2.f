       open(10,status="foo",err=100)
       call abort
  100  continue
       open(10,status="scratch")
       end
