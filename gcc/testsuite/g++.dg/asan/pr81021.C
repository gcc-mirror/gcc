// { dg-do run }

#include <string>

struct ConfigFile {
    ConfigFile(std::string filename, std::string delimiter) { throw "error"; }
    ConfigFile(std::string filename) {}
};

struct Configuration {
    ConfigFile _configFile;

    Configuration(const std::string &root, const char *baseName) 
        : _configFile(root + baseName, "=") { }
    Configuration(const std::string &root, const char *a, const char *b) 
        : _configFile(root + a + b) { }
};


void test() {
    std::string root("etc");
    try {
        Configuration config(root, "notthere");
    }
    catch (...) {
        // exception is thrown, caught here and ignored...
    }
    Configuration config(root, "a", "b"); // ASAN error during constructor here
}

int main(int argc, const char *argv[]) {
    test();
}
