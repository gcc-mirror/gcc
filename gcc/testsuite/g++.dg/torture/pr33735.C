// { dg-do compile }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }
#include <string>
typedef struct _ts { } PyThreadState;
PyThreadState * Py_NewInterpreter(void);
void Py_EndInterpreter(PyThreadState *);
class ApplicationError {
public:
    ApplicationError(std::string errormsg) : errormsg(errormsg)  { }
    std::string errormsg;
};
void run()
{
    PyThreadState *py_state=__null;
    try {
        if (!(py_state=Py_NewInterpreter()))
            throw ApplicationError("error");
    }
    catch(ApplicationError e) {
        Py_EndInterpreter(py_state);
    }
}
