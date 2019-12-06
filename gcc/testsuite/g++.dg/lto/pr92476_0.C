// { dg-lto-do link }
// { dg-lto-options { { -O0 -flto -shared -fPIC -fvisibility=hidden } } }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }
// { dg-extra-ld-options "-shared" }

namespace Passenger {
namespace Json {
class Value {};
} // namespace Json
namespace ConfigKit {
class Translator {};
} // namespace ConfigKit
namespace LoggingKit {
void initialize(const Json::Value & = Json::Value(),
                const ConfigKit::Translator & = ConfigKit::Translator());
void init_module() { initialize(); }
} // namespace LoggingKit
} // namespace Passenger

