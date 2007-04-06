-- { dg-do run }

procedure interface2 is
 package Types is
    type Iface     is synchronized interface;
    type Any_Iface is access all Iface'Class;
--  
    protected type T_PO (S : Integer) is new Iface with end;
    task type T_Task    (R : Any_Iface);
--  
    Obj_1 : aliased T_PO (0);
    Obj_2 : T_Task (Obj_1'Access); --  Test
  end Types;
--
  package body Types is
    protected body T_PO is end;
    task body T_Task    is begin null; end;
  end Types;
--
begin
   null;
end;
