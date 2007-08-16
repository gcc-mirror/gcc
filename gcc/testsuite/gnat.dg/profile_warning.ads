pragma Profile_Warnings (Ravenscar);
with profile_warning_p;
package profile_warning is
   pragma Elaborate_Body;
   procedure I is new profile_warning_p.Proc;
end;
