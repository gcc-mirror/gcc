package Sync_Iface_Call_Pkg is

   type IFace is synchronized interface;

   procedure Do_Stuff
     (This  : in out IFace;
      Value : in Integer) is null;

   procedure Do_Stuff_Access
     (This  : in out IFace;
      Value : not null access Integer) is null;

   procedure Do_Stuff_2
     (This  : not null access IFace;
      Value : in Integer) is null;

   procedure Do_Stuff_2_Access
     (This  : not null access IFace;
      Value : not null access Integer) is null;

end Sync_Iface_Call_Pkg;
