// { dg-do assemble  }
// GROUPS passed miscellaneous-bugs
typedef int va_list;
class ostream;
class String {
public:
operator const char *() const;
};
class FwtErrorManager {
public:
    FwtErrorManager(ostream& err, const char *program);
public:
    void form(const char *format, ...);
protected:
    const String _program;	 
private:
    FwtErrorManager(const FwtErrorManager&);
    void operator=(const FwtErrorManager&);
};
class FwtProgram: public FwtErrorManager {
public:
    FwtProgram();	 
};
class FwtArgOptions { };
class FwtStdProgram: public FwtProgram, public FwtArgOptions {
public:
    FwtStdProgram();	 
    void usage_if_not_complete();
};
void
FwtStdProgram::usage_if_not_complete()
{
	FwtStdProgram& thisp = *this;
	thisp.form("%s: error, there were unrecognized options",
		   (char *) FwtErrorManager::_program);// { dg-error "" } .*
}
