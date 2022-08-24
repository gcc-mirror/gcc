// { dg-do compile }

struct ControlClass
{
    virtual ~ControlClass();

    int Width;
    int Height;
    unsigned IsToRepaint : 1;
};

struct SelectClass : ControlClass
{
    SelectClass(void);
};

int Non_Folded_Value();

SelectClass::SelectClass(void)
{
    int factor = Non_Folded_Value();
    Width = 32 << factor;
    Height = 24 << factor;
}
