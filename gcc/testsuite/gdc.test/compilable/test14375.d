/*
TEST_OUTPUT:
---
---
 */
interface IKeysAPI(string greetings) {
    static assert(greetings == "Hello world", greetings);
}

void main() {
    foreach (method; __traits(allMembers, IKeysAPI!("Hello world"))) {
        static assert (method.length, "Empty string from the compiler ??");
        pragma(msg, method);
    }
}
