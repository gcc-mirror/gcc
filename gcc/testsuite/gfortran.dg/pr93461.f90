! { dg-do compile }
! PR fortran/93461
module aModuleWithAnAllowedName
  interface
     module subroutine aShortName()
     end subroutine aShortName
  end interface
end module aModuleWithAnAllowedName

submodule (aModuleWithAnAllowedName) aSubmoduleWithAVeryVeryVeryLongButEntirelyLegalName
contains
  subroutine aShortName()
    call aSubroutineWithAVeryLongNameThatWillCauseAProblem()
    call aSubroutineWithAVeryLongNameThatWillCauseAProblemAlso()
  end subroutine aShortName
  
  subroutine aSubroutineWithAVeryLongNameThatWillCauseAProblem()
  end subroutine aSubroutineWithAVeryLongNameThatWillCauseAProblem

  subroutine aSubroutineWithAVeryLongNameThatWillCauseAProblemAlso()
  end subroutine aSubroutineWithAVeryLongNameThatWillCauseAProblemAlso  
end submodule aSubmoduleWithAVeryVeryVeryLongButEntirelyLegalName
