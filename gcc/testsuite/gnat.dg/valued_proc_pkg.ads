pragma Extend_System (Aux_DEC);
with System; use System;

package Valued_Proc_Pkg is

    procedure GETMSG (STATUS : out UNSIGNED_LONGWORD;
                      MSGLEN : out UNSIGNED_WORD);

    pragma Interface (EXTERNAL, GETMSG);

    pragma IMPORT_VALUED_PROCEDURE (GETMSG, "SYS$GETMSG",
                                    (UNSIGNED_LONGWORD, UNSIGNED_WORD),
                                    (VALUE, REFERENCE));

end Valued_Proc_Pkg;
