enum Lisp_Type
{
  Lisp_Int                     
  ,Lisp_Record                 
  ,Lisp_Cons                   
  ,Lisp_String                 
  ,Lisp_Vector                 
  ,Lisp_Symbol
  ,Lisp_Char                     
};
typedef
union Lisp_Object
  {
    struct
      {
        enum Lisp_Type type: 3L ;
        unsigned long  markbit: 1;
        unsigned long  val: 60;
      } gu;
    long  i;
  }
Lisp_Object;
extern int initialized;
void
init_device_faces (int *d)
{
  if (initialized)
    {
      Lisp_Object tdevice;
      do {
          tdevice = (union Lisp_Object)
                        { gu:
                          { markbit: 0,
                            type: Lisp_Record,
                            val: ((unsigned long )d)
                          }
                        };
      } while (0);
      call_critical_lisp_code (tdevice);
    }
}
