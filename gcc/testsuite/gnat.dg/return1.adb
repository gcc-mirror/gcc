-- { dg-do compile }
-- { dg-options "-gnatwa" }

package body return1 is
    function X_Func (O : access Child) return access Base'Class is
    begin
       return X_Local : access Base'Class do
          X_Local := O;
       end return;
    end X_Func;
end return1;
