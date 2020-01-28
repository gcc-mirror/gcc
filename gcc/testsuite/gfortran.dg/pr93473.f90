! { dg-do compile }
! { dg-options "-ffree-line-length-none" }
! PR fortran/93473
module aModestlyLongModuleName
  
  type :: aTypeWithASignificantlyLongNameButStillAllowedOK
  end type aTypeWithASignificantlyLongNameButStillAllowedOK
  
  interface
     module function aFunctionWithALongButStillAllowedName(parameters) result(self)
       type(aTypeWithASignificantlyLongNameButStillAllowedOK) :: self
     end function aFunctionWithALongButStillAllowedName
  end interface
  
end module aModestlyLongModuleName

submodule (aModestlyLongModuleName) aTypeWithASignificantlyLongNameButStillAllowedOK_

contains

  module procedure aFunctionWithALongButStillAllowedName
     class(*), pointer :: genericObject
  end procedure aFunctionWithALongButStillAllowedName

end submodule aTypeWithASignificantlyLongNameButStillAllowedOK_

submodule (aModestlyLongModuleName:aTypeWithASignificantlyLongNameButStillAllowedOK_) aSubmoduleWithASignificantlyLongButStillAllowedName__
end submodule aSubmoduleWithASignificantlyLongButStillAllowedName__
