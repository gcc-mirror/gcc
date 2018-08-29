-- { dg-do compile }
-- { dg-options "-g -fgnat-encodings=minimal" }

procedure Debug14 is

  type Db_Kind_T is (Raw, Relational, Object);

  type Db_Model_T (Kind : Db_Kind_T) is
    record

      case Kind is

        when Raw =>
          Fs_Type : Integer;

        when Relational | Object =>
          Vendor_Id : Integer;

          case Kind is
            when Relational =>
              N_Tables : Integer;

            when others =>
              null;
          end case;

      end case;

    end record;

  type Raw_Db_T is new Db_Model_T (Kind => Raw);
  type Raw_Db_P is access Raw_Db_T;

  Db : Raw_Db_P := new Raw_Db_T;

begin
  null;
end;
