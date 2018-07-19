--  { dg-do compile }

with BIP_Case_Expr_Pkg; use BIP_Case_Expr_Pkg;

procedure BIP_Case_Expr is
   function Make_Any_Lim_Ctrl (Flag : Boolean) return Lim_Ctrl is
   begin
      return (case Flag is
                 when True  => Make_Lim_Ctrl,
                 when False => Make_Lim_Ctrl);
   end;

   Res : Lim_Ctrl := Make_Any_Lim_Ctrl (True);

begin null; end BIP_Case_Expr;
