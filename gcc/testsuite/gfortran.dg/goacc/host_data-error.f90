! { dg-do compile }

subroutine foo ()
!$acc host_data ! { dg-error "'host_data' construct at .1. requires 'use_device' clause" }
!$acc end host_data
end
