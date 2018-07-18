package Overriding_Ops2_Pkg.High is
   type High_Level_Session is new Session_Type with private;
private
   type High_Level_Session is new Session_Type with null record;
end Overriding_Ops2_Pkg.High;
