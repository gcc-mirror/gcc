package main

import target "go1"
import "testing"
import "regexp"

var tests = []testing.InternalTest{
}

var benchmarks = []testing.InternalBenchmark{
	{"go1.BenchmarkBinaryTree17", target.BenchmarkBinaryTree17},
	{"go1.BenchmarkFannkuch11", target.BenchmarkFannkuch11},
	{"go1.BenchmarkGobDecode", target.BenchmarkGobDecode},
	{"go1.BenchmarkGobEncode", target.BenchmarkGobEncode},
	{"go1.BenchmarkGzip", target.BenchmarkGzip},
	{"go1.BenchmarkGunzip", target.BenchmarkGunzip},
	{"go1.BenchmarkJSONEncode", target.BenchmarkJSONEncode},
	{"go1.BenchmarkJSONDecode", target.BenchmarkJSONDecode},
	{"go1.BenchmarkRevcomp25M", target.BenchmarkRevcomp25M},
	{"go1.BenchmarkTemplate", target.BenchmarkTemplate},
}
var examples = []testing.InternalExample{}

var matchPat string
var matchRe *regexp.Regexp

func matchString(pat, str string) (result bool, err error) {
	if matchRe == nil || matchPat != pat {
		matchPat = pat
		matchRe, err = regexp.Compile(matchPat)
		if err != nil {
			return
		}
	}
	return matchRe.MatchString(str), nil
}

func main() {
	testing.Main(matchString, tests, benchmarks, examples)
}
