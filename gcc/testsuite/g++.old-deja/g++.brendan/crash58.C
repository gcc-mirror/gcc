// Build don't link: 
// GROUPS passed old-abort
class X {
public:
void doit();
};

X::::doit()// ERROR -  (syntax|parse) error.*
{
}
