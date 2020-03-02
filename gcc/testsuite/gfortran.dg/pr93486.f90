! { dg-do compile }
! PR fortran/93486
module ivs
  interface l
     module procedure l_
  end interface l
contains
  function l_()
  end function l_
end module ivs

module aModeratleyLongModuleName
  use ivs
  interface
     module subroutine cmo()
     end subroutine cmo
  end interface
end module aModeratleyLongModuleName

submodule (aModeratleyLongModuleName) aNameForASubmoduleThatIsVeryLongButWhichIsLegalStill
contains
  module procedure cmo
  end procedure cmo
end submodule aNameForASubmoduleThatIsVeryLongButWhichIsLegalStill

submodule (aModeratleyLongModuleName:aNameForASubmoduleThatIsVeryLongButWhichIsLegalStill) sb
end submodule sb

submodule (aModeratleyLongModuleName:sb) sc
end submodule sc
