// prms-id: 5274
class VHDLIdentifier;

class VHDLPackageProtoRep {
public:
    int thing();
private:
    virtual VHDLIdentifier &actual_name() ;
};
extern void form(const char *format, ... );
int
VHDLPackageProtoRep::thing()
{
    form("package `%s'", (char *)actual_name()); // ERROR - can't convert from incomplete type
    return 0;
}
