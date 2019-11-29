// { dg-options { -O2 -flto -shared -fPIC -fvisibility=hidden } }

namespace Passenger {
namespace Json {
class Value;
}
namespace ConfigKit {
class Translator;
}
namespace LoggingKit {
void initialize(const Json::Value &, const ConfigKit::Translator &) {}
} // namespace LoggingKit
} // namespace Passenger
