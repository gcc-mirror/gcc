package Task7_Pkg is

  task type Task_Typ is
  end Task_Typ;

  type Task_Wrapper is tagged limited record
    Tsk : Task_Typ;
  end record;

  type Ext is new Task_Wrapper with record
    I : Integer;
  end record;

  type Ptr is access Ext;

end Task7_Pkg;
