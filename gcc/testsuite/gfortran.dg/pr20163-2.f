       open(10,status="foo",err=100) ! { dg-warning "STATUS specifier in OPEN statement .* has invalid value" }
       call abort
  100  continue
       open(10,status="scratch")
       end
