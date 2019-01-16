module imports.testkwd;

/****************************************/

version(Windows) enum sep = "\\";  else enum sep = "/";

enum thatFile = "runnable"~sep~"imports"~sep~"testkwd_file.d";
enum thatMod  = "imports.testkwd";
//enum thatLine;
enum thatFunc  = "imports.testkwd.getCalleeFunc";
enum thatFunc2 = `string imports.testkwd.getCalleeFunc2(int x = 1, string s = "hello")`;

string getCalleeFile() { return __FILE__; }
ulong  getCalleeLine() { return __LINE__; }         enum thatLine = 14;
string getCalleeMod()  { return __MODULE__; }
string getCalleeFunc() { return __FUNCTION__; }
string getCalleeFunc2(int x = 1, string s = "hello") { return __PRETTY_FUNCTION__; }

/****************************************/

string getFuncArgFile (string name = __FILE__           ) { return name; }
ulong  getFuncArgLine (ulong  lnum = __LINE__           ) { return lnum; }
string getFuncArgMod  (string name = __MODULE__         ) { return name; }
string getFuncArgFunc (string name = __FUNCTION__       ) { return name; }
string getFuncArgFunc2(string name = __PRETTY_FUNCTION__) { return name; }

string getFuncTiargFile (string name = __FILE__           )() { return name; }
ulong  getFuncTiargLine (ulong  lnum = __LINE__           )() { return lnum; }
string getFuncTiargMod  (string name = __MODULE__         )() { return name; }
string getFuncTiargFunc (string name = __FUNCTION__       )() { return name; }
string getFuncTiargFunc2(string name = __PRETTY_FUNCTION__)() { return name; }

template getInstTiargFile (string name = __FILE__           ) { enum getInstTiargFile  = name; }
template getInstTiargLine (ulong  lnum = __LINE__           ) { enum getInstTiargLine  = lnum; }
template getInstTiargMod  (string name = __MODULE__         ) { enum getInstTiargMod   = name; }
template getInstTiargFunc (string name = __FUNCTION__       ) { enum getInstTiargFunc  = name; }
template getInstTiargFunc2(string name = __PRETTY_FUNCTION__) { enum getInstTiargFunc2 = name; }
