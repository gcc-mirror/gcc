--  { dg-do compile }
package body TF_Interface_1 is
   procedure Get_It (Handle : Stream_Access; It : out CF_Interface_1'class)
  is
  begin
     CF_Interface_1'Class'Read (Handle, It);
  end;
end;
